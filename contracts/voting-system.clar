;; title: voting-system
;; version: 1.0.0
;; summary: Transparent election system with anonymous ballot casting and auditable results
;; description: Provides secure voting with privacy-preserving audit trails and real-time result tracking

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u2000))
(define-constant ERR_VOTING_CLOSED (err u2001))
(define-constant ERR_ALREADY_VOTED (err u2002))
(define-constant ERR_INVALID_CANDIDATE (err u2003))
(define-constant ERR_NOT_REGISTERED (err u2004))
(define-constant ERR_ELECTION_NOT_STARTED (err u2005))
(define-constant ERR_ELECTION_ENDED (err u2006))

;; data vars
(define-data-var election-name (string-ascii 100) "")
(define-data-var voting-open bool false)
(define-data-var election-start-block uint u0)
(define-data-var election-end-block uint u0)
(define-data-var total-votes uint u0)
(define-data-var voter-registry-contract principal 'ST000000000000000000002AMW42H.voter-registry)

;; data maps
(define-map candidates uint 
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint
  }
)

(define-map candidate-index (string-ascii 50) uint)
(define-map ballot-hashes (buff 32) 
  {
    voter-hash: (buff 32),
    candidate-id: uint,
    vote-block: uint,
    ballot-id: uint
  }
)

(define-map voter-ballots principal uint)
(define-data-var next-candidate-id uint u1)
(define-data-var next-ballot-id uint u1)

;; public functions
(define-public (initialize-election (name (string-ascii 100)) (duration-blocks uint))
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get voting-open)) ERR_VOTING_CLOSED)
    (var-set election-name name)
    (var-set election-start-block stacks-block-height)
    (var-set election-end-block (+ stacks-block-height duration-blocks))
    (ok true)
  )
)

(define-public (add-candidate (name (string-ascii 50)) (description (string-ascii 200)))
  (let 
    (
      (candidate-id (var-get next-candidate-id))
    )
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (asserts! (not (var-get voting-open)) ERR_VOTING_CLOSED)
    (asserts! (is-none (map-get? candidate-index name)) ERR_INVALID_CANDIDATE)
    
    (map-set candidates candidate-id
      {
        name: name,
        description: description,
        vote-count: u0
      }
    )
    (map-set candidate-index name candidate-id)
    (var-set next-candidate-id (+ candidate-id u1))
    (ok candidate-id)
  )
)

(define-public (start-voting)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (asserts! (>= stacks-block-height (var-get election-start-block)) ERR_ELECTION_NOT_STARTED)
    (var-set voting-open true)
    (ok true)
  )
)

(define-public (end-voting)
  (begin
    (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
    (var-set voting-open false)
    (ok true)
  )
)

(define-public (cast-vote (candidate-id uint) (voter-hash (buff 32)))
  (let 
    (
      (ballot-id (var-get next-ballot-id))
      (ballot-hash (sha256 (concat voter-hash (buff-from-uint-be candidate-id))))
    )
    (asserts! (var-get voting-open) ERR_VOTING_CLOSED)
    (asserts! (<= stacks-block-height (var-get election-end-block)) ERR_ELECTION_ENDED)
    (asserts! (is-some (map-get? candidates candidate-id)) ERR_INVALID_CANDIDATE)
    (asserts! (is-none (map-get? voter-ballots tx-sender)) ERR_ALREADY_VOTED)
    
    ;; Record anonymous ballot
    (map-set ballot-hashes ballot-hash
      {
        voter-hash: voter-hash,
        candidate-id: candidate-id,
        vote-block: stacks-block-height,
        ballot-id: ballot-id
      }
    )
    
    ;; Update candidate vote count
    (match (map-get? candidates candidate-id)
      candidate-data 
      (map-set candidates candidate-id
        (merge candidate-data { vote-count: (+ (get vote-count candidate-data) u1) })
      )
      false
    )
    
    ;; Mark voter as having voted
    (map-set voter-ballots tx-sender ballot-id)
    (var-set next-ballot-id (+ ballot-id u1))
    (var-set total-votes (+ (var-get total-votes) u1))
    
    (ok ballot-hash)
  )
)

;; read only functions
(define-read-only (get-election-info)
  {
    name: (var-get election-name),
    voting-open: (var-get voting-open),
    start-block: (var-get election-start-block),
    end-block: (var-get election-end-block),
    total-votes: (var-get total-votes),
    current-block: stacks-block-height
  }
)

(define-read-only (get-candidate (candidate-id uint))
  (map-get? candidates candidate-id)
)

(define-read-only (get-candidate-by-name (name (string-ascii 50)))
  (match (map-get? candidate-index name)
    id (map-get? candidates id)
    none
  )
)

(define-read-only (get-ballot-info (ballot-hash (buff 32)))
  (map-get? ballot-hashes ballot-hash)
)

(define-read-only (has-voter-voted (voter principal))
  (is-some (map-get? voter-ballots voter))
)

(define-read-only (get-results-summary)
  (let 
    (
      (candidate-1 (default-to { name: "", description: "", vote-count: u0 } (map-get? candidates u1)))
      (candidate-2 (default-to { name: "", description: "", vote-count: u0 } (map-get? candidates u2)))
      (candidate-3 (default-to { name: "", description: "", vote-count: u0 } (map-get? candidates u3)))
    )
    {
      election-name: (var-get election-name),
      total-votes: (var-get total-votes),
      voting-status: (var-get voting-open),
      candidates: {
        candidate-1: candidate-1,
        candidate-2: candidate-2,
        candidate-3: candidate-3
      }
    }
  )
)

(define-read-only (verify-ballot (ballot-hash (buff 32)) (voter-hash (buff 32)) (candidate-id uint))
  (let 
    (
      (expected-hash (sha256 (concat voter-hash (buff-from-uint-be candidate-id))))
    )
    (and 
      (is-eq ballot-hash expected-hash)
      (is-some (map-get? ballot-hashes ballot-hash))
    )
  )
)

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (buff-from-uint-be (value uint))
  (unwrap-panic (to-consensus-buff? value))
)
