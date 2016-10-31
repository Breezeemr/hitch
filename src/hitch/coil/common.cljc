(ns hitch.coil.common
  (:require [hitch.protocol.selector :as psel]
            [hitch.selector.common :refer [resolution-value
                                           get-selector-resolution]]
            [hitch.protocol.tracking :as ptrack]
            [hitch.tracking.common :as track]
            [hitch.tracking.halt :as halt]
            [hitch.protocol.graph :as pgraph]
            [hitch.util :refer [throw-ex-info rsome UNKNOWN unknown?]]))

(defn coil-kind [coil]
  (cond
    (psel/selector? coil)
    :hitch.coil.kind/selector

    (get coil :hitch.coil.braid/name)
    :hitch.coil.kind/braid

    :else
    (if-some [kind (-> coil meta :hitch.coil/fn-kind)]
      kind
      (when (fn? coil)
        :hitch.coil.kind/haltable))))

(defn unbraid [braid context]
  (or (when-some [impl-hint (:hitch.coil.braid/coil-hint braid)]
        (when-some [contextual-preferred (context impl-hint)]
          (or (find braid contextual-preferred)
            (throw-ex-info "Coil's contextually-preferred impl does not exist."
              braid)))
        (when-some [default-preferred (:default impl-hint)]
          (or (find braid default-preferred)
            (throw-ex-info "Coil's default preferred impl does not exist."
              braid))))
    (rsome #(find braid %)
      (if (= context :hitch.coil.context/tracking-getter)
        [:hitch.coil.kind/selector
         :hitch.coil.kind/selector-provider
         :hitch.coil.kind/haltable
         :hitch.coil.kind/resolver]
        [:hitch.coil.kind/selector
         :hitch.coil.kind/selector-provider
         :hitch.coil.kind/resolver
         :hitch.coil.kind/haltable]))
    (throw-ex-info "Coil has no implementations." braid)))

;; Typical values for context
;; :hitch.coil.context/tracking-getter
;;     (used by dget and select, prefers haltables to resolvers because it can reuse
;;      a tracker and halting-context.)
;; These prefer resolvers to haltables because they
;; :hitch.coil.context/selector
;; :hitch.coil.context/anchor
;; INVARIANT: never returns a braid
(defn uncoil [coil context]
  (let [kind (coil-kind coil)]
    (case kind
      :hitch.coil.kind/braid (unbraid coil context)

      nil (throw-ex-info "Could not uncoil value." coil)

      [kind coil])))

(defn dget-coil* [unknown kind impl tracker args]
  (case kind
    :hitch.coil.kind/selector
    (ptrack/-depend-sel! tracker unknown impl)

    :hitch.coil.kind/selector-provider
    (let [sel (apply impl args)]
      (when-not (psel/selector? sel)
        (throw-ex-info "Selector-provider did not return selector!" impl))
      (ptrack/-depend-sel! tracker unknown sel))

    :hitch.coil.kind/resolver
    (let [res (apply impl tracker args)]
      (resolution-value res unknown))

    :hitch.coil.kind/haltable
    (halt/dget-haltable unknown impl tracker args)))

(defn dget-coil [context unknown coil tracker args]
  (let [[kind impl] (uncoil coil context)]
    (dget-coil* unknown kind impl tracker args)))

(defn resolve-coil* [kind impl lookup args]
  (case kind
    :hitch.coil.kind/selector
    (get-selector-resolution lookup impl)

    :hitch.coil.kind/selector-provider
    (let [sel (apply impl args)]
      (when-not (psel/selector? sel)
        (throw-ex-info "Selector-provider did not return selector!" impl))
      (get-selector-resolution lookup sel))

    :hitch.coil.kind/resolver
    (apply impl (track/simple-tracker lookup) args)

    :hitch.coil.kind/haltable
    (halt/resolve-haltable impl lookup args)))

(defn resolve-coil [context coil lookup args]
  (let [[kind impl] (uncoil coil context)]
    (resolve-coil* kind impl lookup args)))

(defn optimized-invoke-braid!
  "Run a optimal coil in a braid with the provided ILookupTracking and context.
  Lookup tracker must implement BraidOptimizerProvider.
  If value is not present or unresolved, return unknown."
  [context unknown braid lookup-tracker&optimizer-provider args]
  (let [opt (pgraph/-braid-optimizer lookup-tracker&optimizer-provider)]
    (ptrack/-depend-braid! opt context braid unknown
      lookup-tracker&optimizer-provider args)))
