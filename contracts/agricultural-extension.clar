;; Agricultural Extension Services Contract
;; A smart contract system for farmer education, expert consultation, and resource coordination

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-SESSION-EXPIRED (err u105))

;; Contract Owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data Variables
(define-data-var expert-registration-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var consultation-fee uint u500000) ;; 0.5 STX in microSTX
(define-data-var next-expert-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-resource-id uint u1)

;; Data Maps
(define-map experts uint {
  address: principal,
  name: (string-utf8 100),
  specialization: (string-utf8 100),
  rating: uint,
  total-sessions: uint,
  active: bool
})

(define-map consultation-sessions uint {
  farmer: principal,
  expert-id: uint,
  problem: (string-utf8 500),
  status: (string-ascii 20),
  created-at: uint,
  expires-at: uint,
  payment-amount: uint
})

(define-map best-practices uint {
  title: (string-utf8 100),
  content: (string-utf8 1000),
  category: (string-utf8 50),
  author: principal,
  created-at: uint,
  upvotes: uint
})

(define-map farmer-profiles principal {
  name: (string-utf8 100),
  location: (string-utf8 100),
  farm-size: uint,
  crop-types: (string-utf8 200),
  joined-at: uint
})

(define-map resource-coordination uint {
  title: (string-utf8 100),
  description: (string-utf8 500),
  resource-type: (string-utf8 50),
  quantity-available: uint,
  location: (string-utf8 100),
  coordinator: principal,
  active: bool
})

;; Public Functions

;; Register as an expert in the extension system
(define-public (register-expert (name (string-utf8 100)) (specialization (string-utf8 100)))
  (let ((expert-id (var-get next-expert-id)))
    (asserts! (>= (stx-get-balance tx-sender) (var-get expert-registration-fee)) ERR-INSUFFICIENT-PAYMENT)
    (try! (stx-transfer? (var-get expert-registration-fee) tx-sender CONTRACT-OWNER))
    (map-set experts expert-id {
      address: tx-sender,
      name: name,
      specialization: specialization,
      rating: u5,
      total-sessions: u0,
      active: true
    })
    (var-set next-expert-id (+ expert-id u1))
    (ok expert-id)
  )
)

;; Register as a farmer
(define-public (register-farmer (name (string-utf8 100)) (location (string-utf8 100)) (farm-size uint) (crop-types (string-utf8 200)))
  (begin
    (asserts! (is-none (map-get? farmer-profiles tx-sender)) ERR-ALREADY-EXISTS)
    (map-set farmer-profiles tx-sender {
      name: name,
      location: location,
      farm-size: farm-size,
      crop-types: crop-types,
      joined-at: stacks-block-height
    })
    (ok true)
  )
)

;; Request expert consultation
(define-public (request-consultation (expert-id uint) (problem (string-utf8 500)))
  (let 
    ((session-id (var-get next-session-id))
     (fee (var-get consultation-fee)))
    (asserts! (is-some (map-get? experts expert-id)) ERR-NOT-FOUND)
    (asserts! (>= (stx-get-balance tx-sender) fee) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (is-some (map-get? farmer-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? fee tx-sender CONTRACT-OWNER))
    (map-set consultation-sessions session-id {
      farmer: tx-sender,
      expert-id: expert-id,
      problem: problem,
      status: "pending",
      created-at: stacks-block-height,
      expires-at: (+ stacks-block-height u144), ;; ~24 hours
      payment-amount: fee
    })
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

;; Accept consultation request (expert only)
(define-public (accept-consultation (session-id uint))
  (let ((session (unwrap! (map-get? consultation-sessions session-id) ERR-NOT-FOUND)))
    (let ((expert (unwrap! (map-get? experts (get expert-id session)) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get address expert)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status session) "pending") ERR-INVALID-INPUT)
      (asserts! (< stacks-block-height (get expires-at session)) ERR-SESSION-EXPIRED)
      (map-set consultation-sessions session-id (merge session { status: "active" }))
      (ok true)
    )
  )
)

;; Complete consultation and rate expert
(define-public (complete-consultation (session-id uint) (rating uint))
  (let ((session (unwrap! (map-get? consultation-sessions session-id) ERR-NOT-FOUND)))
    (let ((expert (unwrap! (map-get? experts (get expert-id session)) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get farmer session)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status session) "active") ERR-INVALID-INPUT)
      (asserts! (and (>= rating u1) (<= rating u10)) ERR-INVALID-INPUT)
      ;; Update session
      (map-set consultation-sessions session-id (merge session { status: "completed" }))
      ;; Update expert stats
      (let ((new-total (+ (get total-sessions expert) u1))
            (new-rating (/ (+ (* (get rating expert) (get total-sessions expert)) rating) new-total)))
        (map-set experts (get expert-id session) (merge expert {
          rating: new-rating,
          total-sessions: new-total
        }))
      )
      ;; Transfer payment to expert
      (try! (as-contract (stx-transfer? (get payment-amount session) tx-sender (get address expert))))
      (ok true)
    )
  )
)

;; Share best practices
(define-public (share-best-practice (title (string-utf8 100)) (content (string-utf8 1000)) (category (string-utf8 50)))
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (is-some (map-get? farmer-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set best-practices resource-id {
      title: title,
      content: content,
      category: category,
      author: tx-sender,
      created-at: stacks-block-height,
      upvotes: u0
    })
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Upvote best practice
(define-public (upvote-practice (practice-id uint))
  (let ((practice (unwrap! (map-get? best-practices practice-id) ERR-NOT-FOUND)))
    (asserts! (is-some (map-get? farmer-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set best-practices practice-id (merge practice {
      upvotes: (+ (get upvotes practice) u1)
    }))
    (ok true)
  )
)

;; Coordinate resources
(define-public (coordinate-resource (title (string-utf8 100)) (description (string-utf8 500)) (resource-type (string-utf8 50)) (quantity uint) (location (string-utf8 100)))
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (is-some (map-get? farmer-profiles tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set resource-coordination resource-id {
      title: title,
      description: description,
      resource-type: resource-type,
      quantity-available: quantity,
      location: location,
      coordinator: tx-sender,
      active: true
    })
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Read-only Functions

(define-read-only (get-expert (expert-id uint))
  (map-get? experts expert-id)
)

(define-read-only (get-farmer-profile (farmer principal))
  (map-get? farmer-profiles farmer)
)

(define-read-only (get-consultation-session (session-id uint))
  (map-get? consultation-sessions session-id)
)

(define-read-only (get-best-practice (practice-id uint))
  (map-get? best-practices practice-id)
)

(define-read-only (get-resource (resource-id uint))
  (map-get? resource-coordination resource-id)
)

(define-read-only (get-consultation-fee)
  (var-get consultation-fee)
)

(define-read-only (get-expert-registration-fee)
  (var-get expert-registration-fee)
)

