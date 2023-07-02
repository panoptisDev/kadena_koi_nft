(define-data-schema koi-fish-schema
  {"name": string,           ; Name of the Koi Fish
   "description": string,    ; Description of the Koi Fish
   "image": string,          ; IPFS URI of the Koi Fish image
   "attributes": string})    ; Any additional attributes you want to include

(define-namespace (kadenakoi))

(define-keyset 'admin-keyset "YOUR_ADMIN_PUBLIC_KEY")

(define-constant max-supply 3000)

(define-table koi-fish-table
  "koi-fish-id"  ; NFT ID (a unique identifier for each Koi Fish NFT)
  koi-fish-schema)

(define-transaction mint-koi-fish
  (public-key)
  (table-write koi-fish-table
    {"koi-fish-id": (enforce (= 0 (length (filter-keys (at koi-fish-table [])) max-supply)))
     "name": "Kadena Koi Fish"
     "description": "A collection of Koi Fish NFTs on Kadena"
     "image": "IPFS_URI_FOR_KOI_FISH_IMAGE"
     "attributes": "Any additional attributes you want to include"}))

(define-transaction transfer-koi-fish
  (public-key to-public-key koi-fish-id)
  (seq
    (enforce-keyset from-public-key)
    (enforce (is-result (get koi-fish-table {"koi-fish-id": koi-fish-id})))
    (table-delete koi-fish-table {"koi-fish-id": koi-fish-id})
    (table-write koi-fish-table {"koi-fish-id": koi-fish-id "name": (at 'name result) "description": (at 'description result) "image": (at 'image result) "attributes": (at 'attributes result)})
    (enforce-keyset to-public-key)))

(define-read koi-fish-metadata
  (koi-fish-id)
  (at koi-fish-table {"koi-fish-id": koi-fish-id}))
