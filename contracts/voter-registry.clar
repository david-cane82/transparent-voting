;; title: voter-registry
;; version: 1.0.0
;; summary: Manages voter registration and verification for transparent elections
;; description: Provides secure voter registration with unique voter IDs and audit trails

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_ALREADY_REGISTERED (err u1001))
(define-constant ERR_NOT_REGISTERED (err u1002))
(define-constant ERR_REGISTRATION_CLOSED (err u1003))

;; data vars
(define-data-var registration-open bool true)
(define-data-var total-registered uint u0)

;; data maps
(define-map voters principal 
  {
    is-verified: bool,
    registration-block: uint,
    voter-id: (string-ascii 64)
  }
)

(define-map voter-ids (string-ascii 64) principal)

;; public functions
(define-public (register-voter (voter-id (string-ascii 64)))
  (let 
    (
      (current-voter tx-sender)
    )
    (asserts! (var-get registration-open) ERR_REGISTRATION_CLOSED)
    (asserts! (is-none (map-get? voters current-voter)) ERR_ALREADY_REGISTERED)
    (asserts! (is-none (map-get? voter-ids voter-id)) ERR_ALREADY_REGISTERED)
    
    (map-set voters current-voter
      {
        is-verified: true,
        registration-block: stacks-block-height,
        voter-id: voter-id
      }
    )
    (map-set voter-ids voter-id current-voter)
    (var-set total-registered (+ (var-get total-registered) u1))
    (ok voter-id)
  )
)

(define-public (close-registration)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (var-set registration-open false)
    (ok true)
  )
)

(define-public (open-registration)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (var-set registration-open true)
    (ok true)
  )
)

;; read only functions
(define-read-only (is-voter-registered (voter principal))
  (match (map-get? voters voter)
    voter-data (ok (get is-verified voter-data))
    (ok false)
  )
)

(define-read-only (get-voter-info (voter principal))
  (map-get? voters voter)
)

(define-read-only (get-voter-by-id (voter-id (string-ascii 64)))
  (map-get? voter-ids voter-id)
)

(define-read-only (is-registration-open)
  (var-get registration-open)
)

(define-read-only (get-total-registered)
  (var-get total-registered)
)

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)
