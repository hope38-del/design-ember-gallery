;; Design Ember Gallery - Ember Attribution System
;; A blockchain platform for tracking creative attributions and royalty distribution

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_PARAMS (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))

;; Data Variables
(define-data-var next-artwork-id uint u1)
(define-data-var next-ember-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points

;; Data Maps
;; Artwork registry
(define-map artworks
  { artwork-id: uint }
  {
    creator: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    ipfs-hash: (string-ascii 64),
    fingerprint-hash: (string-ascii 64),
    created-at: uint,
    total-embers: uint,
    revenue-generated: uint
  }
)

;; Creation embers - micro-attributions
(define-map creation-embers
  { ember-id: uint }
  {
    source-artwork-id: uint,
    target-artwork-id: uint,
    attribution-weight: uint, ;; Percentage in basis points (e.g., 1000 = 10%)
    ember-type: (string-ascii 32), ;; "inspiration", "collaboration", "reference", "derivative"
    created-by: principal,
    created-at: uint
  }
)

;; Artwork ownership
(define-map artwork-owners
  { artwork-id: uint, owner: principal }
  { balance: uint }
)

;; License NFTs
(define-map artwork-licenses
  { artwork-id: uint }
  {
    license-type: (string-ascii 32),
    commercial-use: bool,
    modification-allowed: bool,
    attribution-required: bool,
    royalty-rate: uint ;; Basis points
  }
)

;; Revenue tracking
(define-map revenue-shares
  { artwork-id: uint, beneficiary: principal }
  { share-percentage: uint, total-earned: uint }
)

;; Public Functions

;; Create new artwork
(define-public (create-artwork 
    (title (string-ascii 128))
    (description (string-ascii 512))
    (ipfs-hash (string-ascii 64))
    (fingerprint-hash (string-ascii 64)))
  (let
    ((artwork-id (var-get next-artwork-id)))
    (begin
      ;; Store artwork data
      (map-set artworks
        { artwork-id: artwork-id }
        {
          creator: tx-sender,
          title: title,
          description: description,
          ipfs-hash: ipfs-hash,
          fingerprint-hash: fingerprint-hash,
          created-at: block-height,
          total-embers: u0,
          revenue-generated: u0
        }
      )
      
      ;; Set initial ownership
      (map-set artwork-owners
        { artwork-id: artwork-id, owner: tx-sender }
        { balance: u100 } ;; 100% initial ownership
      )
      
      ;; Increment next artwork ID
      (var-set next-artwork-id (+ artwork-id u1))
      
      (ok artwork-id)
    )
  )
)

;; Create attribution ember
(define-public (create-ember
    (source-artwork-id uint)
    (target-artwork-id uint)
    (attribution-weight uint)
    (ember-type (string-ascii 32)))
  (let
    ((ember-id (var-get next-ember-id))
     (source-artwork (map-get? artworks { artwork-id: source-artwork-id }))
     (target-artwork (map-get? artworks { artwork-id: target-artwork-id })))
    (begin
      ;; Validate artworks exist
      (asserts! (is-some source-artwork) ERR_NOT_FOUND)
      (asserts! (is-some target-artwork) ERR_NOT_FOUND)
      
      ;; Validate attribution weight (max 10000 basis points = 100%)
      (asserts! (<= attribution-weight u10000) ERR_INVALID_PARAMS)
      
      ;; Store ember
      (map-set creation-embers
        { ember-id: ember-id }
        {
          source-artwork-id: source-artwork-id,
          target-artwork-id: target-artwork-id,
          attribution-weight: attribution-weight,
          ember-type: ember-type,
          created-by: tx-sender,
          created-at: block-height
        }
      )
      
      ;; Update source artwork ember count
      (match source-artwork
        artwork-data (map-set artworks
          { artwork-id: source-artwork-id }
          (merge artwork-data { total-embers: (+ (get total-embers artwork-data) u1) })
        )
        false
      )
      
      ;; Increment next ember ID
      (var-set next-ember-id (+ ember-id u1))
      
      (ok ember-id)
    )
  )
)

;; Set artwork license
(define-public (set-artwork-license
    (artwork-id uint)
    (license-type (string-ascii 32))
    (commercial-use bool)
    (modification-allowed bool)
    (attribution-required bool)
    (royalty-rate uint))
  (let
    ((artwork (map-get? artworks { artwork-id: artwork-id })))
    (begin
      ;; Validate artwork exists
      (asserts! (is-some artwork) ERR_NOT_FOUND)
      
      ;; Only creator can set license
      (asserts! (is-eq tx-sender (get creator (unwrap! artwork ERR_NOT_FOUND))) ERR_UNAUTHORIZED)
      
      ;; Validate royalty rate (max 50% = 5000 basis points)
      (asserts! (<= royalty-rate u5000) ERR_INVALID_PARAMS)
      
      ;; Store license
      (map-set artwork-licenses
        { artwork-id: artwork-id }
        {
          license-type: license-type,
          commercial-use: commercial-use,
          modification-allowed: modification-allowed,
          attribution-required: attribution-required,
          royalty-rate: royalty-rate
        }
      )
      
      (ok true)
    )
  )
)

;; Distribute revenue based on ember attributions
(define-public (distribute-revenue (artwork-id uint) (amount uint))
  (let
    ((artwork (map-get? artworks { artwork-id: artwork-id })))
    (begin
      ;; Validate artwork exists
      (asserts! (is-some artwork) ERR_NOT_FOUND)
      
      ;; Only artwork creator can distribute revenue
      (asserts! (is-eq tx-sender (get creator (unwrap! artwork ERR_NOT_FOUND))) ERR_UNAUTHORIZED)
      
      ;; Calculate platform fee
      (let
        ((platform-fee-amount (/ (* amount (var-get platform-fee)) u10000))
         (distributable-amount (- amount platform-fee-amount)))
        
        ;; Update artwork revenue
        (match artwork
          artwork-data (map-set artworks
            { artwork-id: artwork-id }
            (merge artwork-data { revenue-generated: (+ (get revenue-generated artwork-data) amount) })
          )
          false
        )
        
        ;; Transfer platform fee to contract owner
        (try! (stx-transfer? platform-fee-amount tx-sender CONTRACT_OWNER))
        
        ;; The remaining amount would be distributed to attribution holders
        ;; (Implementation would iterate through embers and calculate shares)
        
        (ok distributable-amount)
      )
    )
  )
)

;; Read-only functions

;; Get artwork details
(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks { artwork-id: artwork-id })
)

;; Get ember details
(define-read-only (get-ember (ember-id uint))
  (map-get? creation-embers { ember-id: ember-id })
)

;; Get artwork license
(define-read-only (get-artwork-license (artwork-id uint))
  (map-get? artwork-licenses { artwork-id: artwork-id })
)

;; Get artwork ownership
(define-read-only (get-artwork-ownership (artwork-id uint) (owner principal))
  (map-get? artwork-owners { artwork-id: artwork-id, owner: owner })
)

;; Get next artwork ID
(define-read-only (get-next-artwork-id)
  (var-get next-artwork-id)
)

;; Get next ember ID
(define-read-only (get-next-ember-id)
  (var-get next-ember-id)
)

;; Admin functions

;; Update platform fee (only contract owner)
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u1000) ERR_INVALID_PARAMS) ;; Max 10%
    (var-set platform-fee new-fee)
    (ok true)
  )
)

;; Utility functions for ember graph traversal
(define-read-only (get-artwork-inspirations (artwork-id uint))
  ;; This would return a list of embers where this artwork is the target
  ;; Implementation would filter embers by target-artwork-id
  (ok artwork-id)
)

(define-read-only (get-artwork-derivatives (artwork-id uint))
  ;; This would return a list of embers where this artwork is the source
  ;; Implementation would filter embers by source-artwork-id
  (ok artwork-id)
)