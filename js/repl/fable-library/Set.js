import{record_type,bool_type,list_type,option_type,class_type}from"./Reflection.js";import{some,value as value_1}from"./Option.js";import{toString,Record}from"./Types.js";import{FSharpList,fold as fold_2,cons,singleton as singleton_1,empty as empty_1,ofArrayWithTail,tail,head,isEmpty as isEmpty_1}from"./List.js";import{fold as fold_1,fill}from"./Array.js";import{structuralHash,partialApply,toIterator,disposeSafe,getEnumerator,isArrayLike}from"./Util.js";import{join}from"./String.js";import{exists as exists_1,cache,forAll as forAll_1,fold as fold_3,reduce,iterate as iterate_1,map as map_1}from"./Seq.js";import{HashSet__get_Comparer,HashSet_$ctor_Z6150332D,HashSet}from"./MutableSet.js";export class SetTreeLeaf$1{constructor(e){this.k=e}}export function SetTreeLeaf$1$reflection(e){return class_type("Set.SetTreeLeaf`1",[e],SetTreeLeaf$1)}export function SetTreeLeaf$1_$ctor_2B595(e){return new SetTreeLeaf$1(e)}export function SetTreeLeaf$1__get_Key(e){return e.k}export class SetTreeNode$1 extends SetTreeLeaf$1{constructor(e,t,r,o){super(e),this.left=t,this.right=r,this.h=0|o}}export function SetTreeNode$1$reflection(e){return class_type("Set.SetTreeNode`1",[e],SetTreeNode$1,SetTreeLeaf$1$reflection(e))}export function SetTreeNode$1_$ctor_5F465FC9(e,t,r,o){return new SetTreeNode$1(e,t,r,o)}export function SetTreeNode$1__get_Left(e){return e.left}export function SetTreeNode$1__get_Right(e){return e.right}export function SetTreeNode$1__get_Height(e){return e.h}export function SetTreeModule_empty(){}export function SetTreeModule_countAux(e,t){e:for(;;){const r=t;if(null!=e){const o=e;if(o instanceof SetTreeNode$1){e=SetTreeNode$1__get_Left(o),t=SetTreeModule_countAux(SetTreeNode$1__get_Right(o),r+1);continue e}return r+1|0}return 0|r}}export function SetTreeModule_count(e){return SetTreeModule_countAux(e,0)}export function SetTreeModule_mk(e,t,r){let o,_;o=null!=e?e instanceof SetTreeNode$1?SetTreeNode$1__get_Height(e):1:0,_=null!=r?r instanceof SetTreeNode$1?SetTreeNode$1__get_Height(r):1:0;const n=0|(o<_?_:o);return 0===n?SetTreeLeaf$1_$ctor_2B595(t):SetTreeNode$1_$ctor_5F465FC9(t,e,r,n+1)}export function SetTreeModule_rebalance(e,t,r){let o,_,n,S,u,a;if(u=null!=e?e instanceof SetTreeNode$1?SetTreeNode$1__get_Height(e):1:0,a=null!=r?r instanceof SetTreeNode$1?SetTreeNode$1__get_Height(r):1:0,a>u+2){const n=value_1(r);if(n instanceof SetTreeNode$1){if(o=SetTreeNode$1__get_Left(n),(null!=o?(_=o,_ instanceof SetTreeNode$1?SetTreeNode$1__get_Height(_):1):0)>u+1){const r=value_1(SetTreeNode$1__get_Left(n));if(r instanceof SetTreeNode$1)return SetTreeModule_mk(SetTreeModule_mk(e,t,SetTreeNode$1__get_Left(r)),SetTreeLeaf$1__get_Key(r),SetTreeModule_mk(SetTreeNode$1__get_Right(r),SetTreeLeaf$1__get_Key(n),SetTreeNode$1__get_Right(n)));throw new Error("internal error: Set.rebalance")}return SetTreeModule_mk(SetTreeModule_mk(e,t,SetTreeNode$1__get_Left(n)),SetTreeLeaf$1__get_Key(n),SetTreeNode$1__get_Right(n))}throw new Error("internal error: Set.rebalance")}if(u>a+2){const o=value_1(e);if(o instanceof SetTreeNode$1){if(n=SetTreeNode$1__get_Right(o),(null!=n?(S=n,S instanceof SetTreeNode$1?SetTreeNode$1__get_Height(S):1):0)>a+1){const e=value_1(SetTreeNode$1__get_Right(o));if(e instanceof SetTreeNode$1)return SetTreeModule_mk(SetTreeModule_mk(SetTreeNode$1__get_Left(o),SetTreeLeaf$1__get_Key(o),SetTreeNode$1__get_Left(e)),SetTreeLeaf$1__get_Key(e),SetTreeModule_mk(SetTreeNode$1__get_Right(e),t,r));throw new Error("internal error: Set.rebalance")}return SetTreeModule_mk(SetTreeNode$1__get_Left(o),SetTreeLeaf$1__get_Key(o),SetTreeModule_mk(SetTreeNode$1__get_Right(o),t,r))}throw new Error("internal error: Set.rebalance")}return SetTreeModule_mk(e,t,r)}export function SetTreeModule_add(e,t,r){if(null!=r){const o=r,_=0|e.Compare(t,SetTreeLeaf$1__get_Key(o));if(o instanceof SetTreeNode$1)return _<0?SetTreeModule_rebalance(SetTreeModule_add(e,t,SetTreeNode$1__get_Left(o)),SetTreeLeaf$1__get_Key(o),SetTreeNode$1__get_Right(o)):0===_?r:SetTreeModule_rebalance(SetTreeNode$1__get_Left(o),SetTreeLeaf$1__get_Key(o),SetTreeModule_add(e,t,SetTreeNode$1__get_Right(o)));{const _=0|e.Compare(t,SetTreeLeaf$1__get_Key(o));return _<0?SetTreeNode$1_$ctor_5F465FC9(t,SetTreeModule_empty(),r,2):0===_?r:SetTreeNode$1_$ctor_5F465FC9(t,r,SetTreeModule_empty(),2)}}return SetTreeLeaf$1_$ctor_2B595(t)}export function SetTreeModule_balance(e,t,r,o){if(null!=t){const _=t;if(null!=o){const n=o;return _ instanceof SetTreeNode$1?n instanceof SetTreeNode$1?SetTreeNode$1__get_Height(_)+2<SetTreeNode$1__get_Height(n)?SetTreeModule_rebalance(SetTreeModule_balance(e,t,r,SetTreeNode$1__get_Left(n)),SetTreeLeaf$1__get_Key(n),SetTreeNode$1__get_Right(n)):SetTreeNode$1__get_Height(n)+2<SetTreeNode$1__get_Height(_)?SetTreeModule_rebalance(SetTreeNode$1__get_Left(_),SetTreeLeaf$1__get_Key(_),SetTreeModule_balance(e,SetTreeNode$1__get_Right(_),r,o)):SetTreeModule_mk(t,r,o):SetTreeModule_add(e,r,SetTreeModule_add(e,SetTreeLeaf$1__get_Key(n),t)):SetTreeModule_add(e,r,SetTreeModule_add(e,SetTreeLeaf$1__get_Key(_),o))}return SetTreeModule_add(e,r,t)}return SetTreeModule_add(e,r,o)}export function SetTreeModule_split(e,t,r){if(null!=r){const o=r;if(o instanceof SetTreeNode$1){const r=0|e.Compare(t,SetTreeLeaf$1__get_Key(o));if(r<0){const r=SetTreeModule_split(e,t,SetTreeNode$1__get_Left(o));return[r[0],r[1],SetTreeModule_balance(e,r[2],SetTreeLeaf$1__get_Key(o),SetTreeNode$1__get_Right(o))]}if(0===r)return[SetTreeNode$1__get_Left(o),!0,SetTreeNode$1__get_Right(o)];{const r=SetTreeModule_split(e,t,SetTreeNode$1__get_Right(o));return[SetTreeModule_balance(e,SetTreeNode$1__get_Left(o),SetTreeLeaf$1__get_Key(o),r[0]),r[1],r[2]]}}{const _=0|e.Compare(SetTreeLeaf$1__get_Key(o),t);return _<0?[r,!1,SetTreeModule_empty()]:0===_?[SetTreeModule_empty(),!0,SetTreeModule_empty()]:[SetTreeModule_empty(),!1,r]}}return[SetTreeModule_empty(),!1,SetTreeModule_empty()]}export function SetTreeModule_spliceOutSuccessor(e){if(null!=e){const t=e;if(t instanceof SetTreeNode$1){if(null==SetTreeNode$1__get_Left(t))return[SetTreeLeaf$1__get_Key(t),SetTreeNode$1__get_Right(t)];{const e=SetTreeModule_spliceOutSuccessor(SetTreeNode$1__get_Left(t));return[e[0],SetTreeModule_mk(e[1],SetTreeLeaf$1__get_Key(t),SetTreeNode$1__get_Right(t))]}}return[SetTreeLeaf$1__get_Key(t),SetTreeModule_empty()]}throw new Error("internal error: Set.spliceOutSuccessor")}export function SetTreeModule_remove(e,t,r){if(null!=r){const o=r,_=0|e.Compare(t,SetTreeLeaf$1__get_Key(o));if(o instanceof SetTreeNode$1){if(_<0)return SetTreeModule_rebalance(SetTreeModule_remove(e,t,SetTreeNode$1__get_Left(o)),SetTreeLeaf$1__get_Key(o),SetTreeNode$1__get_Right(o));if(0===_){if(null==SetTreeNode$1__get_Left(o))return SetTreeNode$1__get_Right(o);if(null==SetTreeNode$1__get_Right(o))return SetTreeNode$1__get_Left(o);{const e=SetTreeModule_spliceOutSuccessor(SetTreeNode$1__get_Right(o));return SetTreeModule_mk(SetTreeNode$1__get_Left(o),e[0],e[1])}}return SetTreeModule_rebalance(SetTreeNode$1__get_Left(o),SetTreeLeaf$1__get_Key(o),SetTreeModule_remove(e,t,SetTreeNode$1__get_Right(o)))}return 0===_?SetTreeModule_empty():r}return r}export function SetTreeModule_mem(e,t,r){e:for(;;){const o=e,_=t;if(null!=r){const n=r,S=0|o.Compare(_,SetTreeLeaf$1__get_Key(n));if(n instanceof SetTreeNode$1){if(S<0){e=o,t=_,r=SetTreeNode$1__get_Left(n);continue e}if(0===S)return!0;e=o,t=_,r=SetTreeNode$1__get_Right(n);continue e}return 0===S}return!1}}export function SetTreeModule_iter(e,t){e:for(;;){const r=e;if(null!=t){const o=t;if(o instanceof SetTreeNode$1){SetTreeModule_iter(r,SetTreeNode$1__get_Left(o)),r(SetTreeLeaf$1__get_Key(o)),e=r,t=SetTreeNode$1__get_Right(o);continue e}r(SetTreeLeaf$1__get_Key(o))}break}}export function SetTreeModule_foldBackOpt(e,t,r){e:for(;;){const o=e,_=r;if(null!=t){const n=t;if(n instanceof SetTreeNode$1){e=o,t=SetTreeNode$1__get_Left(n),r=o(SetTreeLeaf$1__get_Key(n),SetTreeModule_foldBackOpt(o,SetTreeNode$1__get_Right(n),_));continue e}return o(SetTreeLeaf$1__get_Key(n),_)}return _}}export function SetTreeModule_foldBack(e,t,r){return SetTreeModule_foldBackOpt(e,t,r)}export function SetTreeModule_foldOpt(e,t,r){e:for(;;){const o=e,_=t;if(null!=r){const n=r;if(n instanceof SetTreeNode$1){e=o,t=o(SetTreeModule_foldOpt(o,_,SetTreeNode$1__get_Left(n)),SetTreeLeaf$1__get_Key(n)),r=SetTreeNode$1__get_Right(n);continue e}return o(_,SetTreeLeaf$1__get_Key(n))}return _}}export function SetTreeModule_fold(e,t,r){return SetTreeModule_foldOpt(e,t,r)}export function SetTreeModule_forall(e,t){e:for(;;){const r=e;if(null!=t){const o=t;if(o instanceof SetTreeNode$1){if(r(SetTreeLeaf$1__get_Key(o))&&SetTreeModule_forall(r,SetTreeNode$1__get_Left(o))){e=r,t=SetTreeNode$1__get_Right(o);continue e}return!1}return r(SetTreeLeaf$1__get_Key(o))}return!0}}export function SetTreeModule_exists(e,t){e:for(;;){const r=e;if(null!=t){const o=t;if(o instanceof SetTreeNode$1){if(r(SetTreeLeaf$1__get_Key(o))||SetTreeModule_exists(r,SetTreeNode$1__get_Left(o)))return!0;e=r,t=SetTreeNode$1__get_Right(o);continue e}return r(SetTreeLeaf$1__get_Key(o))}return!1}}export function SetTreeModule_subset(e,t,r){return SetTreeModule_forall((t=>SetTreeModule_mem(e,t,r)),t)}export function SetTreeModule_properSubset(e,t,r){return!!SetTreeModule_forall((t=>SetTreeModule_mem(e,t,r)),t)&&SetTreeModule_exists((r=>!SetTreeModule_mem(e,r,t)),r)}export function SetTreeModule_filterAux(e,t,r,o){e:for(;;){const _=e,n=t,S=o;if(null!=r){const u=r;if(u instanceof SetTreeNode$1){const a=n(SetTreeLeaf$1__get_Key(u))?SetTreeModule_add(_,SetTreeLeaf$1__get_Key(u),S):S;e=_,t=n,r=SetTreeNode$1__get_Left(u),o=SetTreeModule_filterAux(_,n,SetTreeNode$1__get_Right(u),a);continue e}return n(SetTreeLeaf$1__get_Key(u))?SetTreeModule_add(_,SetTreeLeaf$1__get_Key(u),S):S}return S}}export function SetTreeModule_filter(e,t,r){return SetTreeModule_filterAux(e,t,r,SetTreeModule_empty())}export function SetTreeModule_diffAux(e,t,r){e:for(;;){const o=e,_=r;if(null==_)return _;if(null!=t){const n=t;if(n instanceof SetTreeNode$1){e=o,t=SetTreeNode$1__get_Left(n),r=SetTreeModule_diffAux(o,SetTreeNode$1__get_Right(n),SetTreeModule_remove(o,SetTreeLeaf$1__get_Key(n),_));continue e}return SetTreeModule_remove(o,SetTreeLeaf$1__get_Key(n),_)}return _}}export function SetTreeModule_diff(e,t,r){return SetTreeModule_diffAux(e,r,t)}export function SetTreeModule_union(e,t,r){if(null!=t){const o=t;if(null!=r){const _=r;if(o instanceof SetTreeNode$1){if(_ instanceof SetTreeNode$1){if(SetTreeNode$1__get_Height(o)>SetTreeNode$1__get_Height(_)){const t=SetTreeModule_split(e,SetTreeLeaf$1__get_Key(o),r);return SetTreeModule_balance(e,SetTreeModule_union(e,SetTreeNode$1__get_Left(o),t[0]),SetTreeLeaf$1__get_Key(o),SetTreeModule_union(e,SetTreeNode$1__get_Right(o),t[2]))}{const r=SetTreeModule_split(e,SetTreeLeaf$1__get_Key(_),t);return SetTreeModule_balance(e,SetTreeModule_union(e,SetTreeNode$1__get_Left(_),r[0]),SetTreeLeaf$1__get_Key(_),SetTreeModule_union(e,SetTreeNode$1__get_Right(_),r[2]))}}return SetTreeModule_add(e,SetTreeLeaf$1__get_Key(_),t)}return SetTreeModule_add(e,SetTreeLeaf$1__get_Key(o),r)}return t}return r}export function SetTreeModule_intersectionAux(e,t,r,o){e:for(;;){const _=e,n=t,S=o;if(null!=r){const u=r;if(u instanceof SetTreeNode$1){const a=SetTreeModule_intersectionAux(_,n,SetTreeNode$1__get_Right(u),S),i=SetTreeModule_mem(_,SetTreeLeaf$1__get_Key(u),n)?SetTreeModule_add(_,SetTreeLeaf$1__get_Key(u),a):a;e=_,t=n,r=SetTreeNode$1__get_Left(u),o=i;continue e}return SetTreeModule_mem(_,SetTreeLeaf$1__get_Key(u),n)?SetTreeModule_add(_,SetTreeLeaf$1__get_Key(u),S):S}return S}}export function SetTreeModule_intersection(e,t,r){return SetTreeModule_intersectionAux(e,r,t,SetTreeModule_empty())}export function SetTreeModule_partition1(e,t,r,o,_){return t(r)?[SetTreeModule_add(e,r,o),_]:[o,SetTreeModule_add(e,r,_)]}export function SetTreeModule_partitionAux(e,t,r,o,_){e:for(;;){const n=e,S=t,u=[o,_];if(null!=r){const a=r;if(a instanceof SetTreeNode$1){const i=SetTreeModule_partitionAux(n,S,SetTreeNode$1__get_Right(a),u[0],u[1]),l=SetTreeModule_partition1(n,S,SetTreeLeaf$1__get_Key(a),i[0],i[1]);e=n,t=S,r=SetTreeNode$1__get_Left(a),o=l[0],_=l[1];continue e}return SetTreeModule_partition1(n,S,SetTreeLeaf$1__get_Key(a),u[0],u[1])}return u}}export function SetTreeModule_partition(e,t,r){return SetTreeModule_partitionAux(e,t,r,SetTreeModule_empty(),SetTreeModule_empty())}export function SetTreeModule_minimumElementAux(e,t){e:for(;;){const r=t;if(null!=e){const r=e;if(r instanceof SetTreeNode$1){e=SetTreeNode$1__get_Left(r),t=SetTreeLeaf$1__get_Key(r);continue e}return SetTreeLeaf$1__get_Key(r)}return r}}export function SetTreeModule_minimumElementOpt(e){if(null!=e){const t=e;return some(t instanceof SetTreeNode$1?SetTreeModule_minimumElementAux(SetTreeNode$1__get_Left(t),SetTreeLeaf$1__get_Key(t)):SetTreeLeaf$1__get_Key(t))}}export function SetTreeModule_maximumElementAux(e,t){e:for(;;){const r=t;if(null!=e){const r=e;if(r instanceof SetTreeNode$1){e=SetTreeNode$1__get_Right(r),t=SetTreeLeaf$1__get_Key(r);continue e}return SetTreeLeaf$1__get_Key(r)}return r}}export function SetTreeModule_maximumElementOpt(e){if(null!=e){const t=e;return some(t instanceof SetTreeNode$1?SetTreeModule_maximumElementAux(SetTreeNode$1__get_Right(t),SetTreeLeaf$1__get_Key(t)):SetTreeLeaf$1__get_Key(t))}}export function SetTreeModule_minimumElement(e){const t=SetTreeModule_minimumElementOpt(e);if(null==t)throw new Error("Set contains no elements");return value_1(t)}export function SetTreeModule_maximumElement(e){const t=SetTreeModule_maximumElementOpt(e);if(null==t)throw new Error("Set contains no elements");return value_1(t)}export class SetTreeModule_SetIterator$1 extends Record{constructor(e,t){super(),this.stack=e,this.started=t}}export function SetTreeModule_SetIterator$1$reflection(e){return record_type("Set.SetTreeModule.SetIterator`1",[e],SetTreeModule_SetIterator$1,(()=>[["stack",list_type(option_type(SetTreeLeaf$1$reflection(e)))],["started",bool_type]]))}export function SetTreeModule_collapseLHS(e){e:for(;;){const t=e;if(isEmpty_1(t))return empty_1();{const r=head(t),o=tail(t);if(null!=r){const _=r;if(_ instanceof SetTreeNode$1){e=ofArrayWithTail([SetTreeNode$1__get_Left(_),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(_)),SetTreeNode$1__get_Right(_)],o);continue e}return t}e=o;continue e}}}export function SetTreeModule_mkIterator(e){return new SetTreeModule_SetIterator$1(SetTreeModule_collapseLHS(singleton_1(e)),!1)}export function SetTreeModule_notStarted(){throw new Error("Enumeration not started")}export function SetTreeModule_alreadyFinished(){throw new Error("Enumeration already started")}export function SetTreeModule_current(e){if(e.started){const t=e.stack;if(isEmpty_1(t))return SetTreeModule_alreadyFinished();if(null!=head(t))return SetTreeLeaf$1__get_Key(head(t));throw new Error("Please report error: Set iterator, unexpected stack for current")}return SetTreeModule_notStarted()}export function SetTreeModule_moveNext(e){if(e.started){const t=e.stack;if(isEmpty_1(t))return!1;if(null!=head(t)){if(head(t)instanceof SetTreeNode$1)throw new Error("Please report error: Set iterator, unexpected stack for moveNext");return e.stack=SetTreeModule_collapseLHS(tail(t)),!isEmpty_1(e.stack)}throw new Error("Please report error: Set iterator, unexpected stack for moveNext")}return e.started=!0,!isEmpty_1(e.stack)}export function SetTreeModule_mkIEnumerator(e){let t=SetTreeModule_mkIterator(e);return{"System.Collections.Generic.IEnumerator`1.get_Current":()=>SetTreeModule_current(t),"System.Collections.IEnumerator.get_Current":()=>SetTreeModule_current(t),"System.Collections.IEnumerator.MoveNext":()=>SetTreeModule_moveNext(t),"System.Collections.IEnumerator.Reset"(){t=SetTreeModule_mkIterator(e)},Dispose(){}}}export function SetTreeModule_compareStacks(e,t,r){e:for(;;){const o=e,_=t,n=r;if(isEmpty_1(_))return isEmpty_1(n)?0:-1;if(isEmpty_1(n))return 1;if(null!=head(n))if(null!=head(_)){const S=head(_),u=head(n);if(S instanceof SetTreeNode$1)if(null==SetTreeNode$1__get_Left(S)){if(!(u instanceof SetTreeNode$1)){const a=0|o.Compare(SetTreeLeaf$1__get_Key(S),SetTreeLeaf$1__get_Key(u));if(0!==a)return 0|a;e=o,t=cons(SetTreeNode$1__get_Right(S),tail(_)),r=cons(SetTreeModule_empty(),tail(n));continue e}if(null==SetTreeNode$1__get_Left(u)){const a=0|o.Compare(SetTreeLeaf$1__get_Key(S),SetTreeLeaf$1__get_Key(u));if(0!==a)return 0|a;e=o,t=cons(SetTreeNode$1__get_Right(S),tail(_)),r=cons(SetTreeNode$1__get_Right(u),tail(n));continue e}{let S,u,a,i,l;switch(isEmpty_1(_)?isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2:null!=head(_)?(S=0,u=tail(_),a=head(_)):isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2,S){case 0:if(a instanceof SetTreeNode$1){e=o,t=ofArrayWithTail([SetTreeNode$1__get_Left(a),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(a),SetTreeModule_empty(),SetTreeNode$1__get_Right(a),0)],u),r=n;continue e}e=o,t=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(a))],u),r=n;continue e;case 1:if(l instanceof SetTreeNode$1){e=o,t=_,r=ofArrayWithTail([SetTreeNode$1__get_Left(l),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(l),SetTreeModule_empty(),SetTreeNode$1__get_Right(l),0)],i);continue e}e=o,t=_,r=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(l))],i);continue e;case 2:throw new Error("unexpected state in SetTree.compareStacks")}}}else{let S,u,a,i,l;switch(isEmpty_1(_)?isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2:null!=head(_)?(S=0,u=tail(_),a=head(_)):isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2,S){case 0:if(a instanceof SetTreeNode$1){e=o,t=ofArrayWithTail([SetTreeNode$1__get_Left(a),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(a),SetTreeModule_empty(),SetTreeNode$1__get_Right(a),0)],u),r=n;continue e}e=o,t=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(a))],u),r=n;continue e;case 1:if(l instanceof SetTreeNode$1){e=o,t=_,r=ofArrayWithTail([SetTreeNode$1__get_Left(l),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(l),SetTreeModule_empty(),SetTreeNode$1__get_Right(l),0)],i);continue e}e=o,t=_,r=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(l))],i);continue e;case 2:throw new Error("unexpected state in SetTree.compareStacks")}}else{if(!(u instanceof SetTreeNode$1)){const a=0|o.Compare(SetTreeLeaf$1__get_Key(S),SetTreeLeaf$1__get_Key(u));if(0!==a)return 0|a;e=o,t=tail(_),r=tail(n);continue e}if(null==SetTreeNode$1__get_Left(u)){const a=0|o.Compare(SetTreeLeaf$1__get_Key(S),SetTreeLeaf$1__get_Key(u));if(0!==a)return 0|a;e=o,t=cons(SetTreeModule_empty(),tail(_)),r=cons(SetTreeNode$1__get_Right(u),tail(n));continue e}{let S,u,a,i,l;switch(isEmpty_1(_)?isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2:null!=head(_)?(S=0,u=tail(_),a=head(_)):isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2,S){case 0:if(a instanceof SetTreeNode$1){e=o,t=ofArrayWithTail([SetTreeNode$1__get_Left(a),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(a),SetTreeModule_empty(),SetTreeNode$1__get_Right(a),0)],u),r=n;continue e}e=o,t=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(a))],u),r=n;continue e;case 1:if(l instanceof SetTreeNode$1){e=o,t=_,r=ofArrayWithTail([SetTreeNode$1__get_Left(l),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(l),SetTreeModule_empty(),SetTreeNode$1__get_Right(l),0)],i);continue e}e=o,t=_,r=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(l))],i);continue e;case 2:throw new Error("unexpected state in SetTree.compareStacks")}}}}else{let S,u,a,i,l;switch(head(n),isEmpty_1(_)?isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2:null!=head(_)?(S=0,u=tail(_),a=head(_)):isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2,S){case 0:if(a instanceof SetTreeNode$1){e=o,t=ofArrayWithTail([SetTreeNode$1__get_Left(a),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(a),SetTreeModule_empty(),SetTreeNode$1__get_Right(a),0)],u),r=n;continue e}e=o,t=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(a))],u),r=n;continue e;case 1:if(l instanceof SetTreeNode$1){e=o,t=_,r=ofArrayWithTail([SetTreeNode$1__get_Left(l),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(l),SetTreeModule_empty(),SetTreeNode$1__get_Right(l),0)],i);continue e}e=o,t=_,r=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(l))],i);continue e;case 2:throw new Error("unexpected state in SetTree.compareStacks")}}else{if(null==head(_)){e=o,t=tail(_),r=tail(n);continue e}{let S,u,a,i,l;switch(head(_),isEmpty_1(_)?isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2:null!=head(_)?(S=0,u=tail(_),a=head(_)):isEmpty_1(n)?S=2:null!=head(n)?(S=1,i=tail(n),l=head(n)):S=2,S){case 0:if(a instanceof SetTreeNode$1){e=o,t=ofArrayWithTail([SetTreeNode$1__get_Left(a),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(a),SetTreeModule_empty(),SetTreeNode$1__get_Right(a),0)],u),r=n;continue e}e=o,t=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(a))],u),r=n;continue e;case 1:if(l instanceof SetTreeNode$1){e=o,t=_,r=ofArrayWithTail([SetTreeNode$1__get_Left(l),SetTreeNode$1_$ctor_5F465FC9(SetTreeLeaf$1__get_Key(l),SetTreeModule_empty(),SetTreeNode$1__get_Right(l),0)],i);continue e}e=o,t=_,r=ofArrayWithTail([SetTreeModule_empty(),SetTreeLeaf$1_$ctor_2B595(SetTreeLeaf$1__get_Key(l))],i);continue e;case 2:throw new Error("unexpected state in SetTree.compareStacks")}}}break}}export function SetTreeModule_compare(e,t,r){return null==t?null==r?0:-1:null==r?1:0|SetTreeModule_compareStacks(e,singleton_1(t),singleton_1(r))}export function SetTreeModule_choose(e){return SetTreeModule_minimumElement(e)}export function SetTreeModule_toList(e){const t=(e,r)=>{e:for(;;){const o=r;if(null!=e){const _=e;if(_ instanceof SetTreeNode$1){e=SetTreeNode$1__get_Left(_),r=cons(SetTreeLeaf$1__get_Key(_),t(SetTreeNode$1__get_Right(_),o));continue e}return cons(SetTreeLeaf$1__get_Key(_),o)}return o}};return t(e,empty_1())}export function SetTreeModule_copyToArray(e,t,r){let o=r;SetTreeModule_iter((e=>{t[o]=e,o=o+1|0}),e)}export function SetTreeModule_toArray(e){const t=0|SetTreeModule_count(e),r=fill(new Array(t),0,t,null);return SetTreeModule_copyToArray(e,r,0),r}export function SetTreeModule_mkFromEnumerator(e,t,r){for(;;){const o=e,_=t,n=r;if(!n["System.Collections.IEnumerator.MoveNext"]())return _;e=o,t=SetTreeModule_add(o,n["System.Collections.Generic.IEnumerator`1.get_Current"](),_),r=n}}export function SetTreeModule_ofArray(e,t){return fold_1(((t,r)=>SetTreeModule_add(e,r,t)),SetTreeModule_empty(),t)}export function SetTreeModule_ofList(e,t){return fold_2(((t,r)=>SetTreeModule_add(e,r,t)),SetTreeModule_empty(),t)}export function SetTreeModule_ofSeq(e,t){if(isArrayLike(t))return SetTreeModule_ofArray(e,t);if(t instanceof FSharpList)return SetTreeModule_ofList(e,t);{const r=getEnumerator(t);try{return SetTreeModule_mkFromEnumerator(e,SetTreeModule_empty(),r)}finally{disposeSafe(r)}}}export class FSharpSet{constructor(e,t){this.comparer=e,this.tree=t}GetHashCode(){return 0|FSharpSet__ComputeHashCode(this)}Equals(e){return e instanceof FSharpSet&&0===SetTreeModule_compare(FSharpSet__get_Comparer(this),FSharpSet__get_Tree(this),FSharpSet__get_Tree(e))}toString(){return"set ["+join("; ",map_1((e=>toString(e)),this))+"]"}get[Symbol.toStringTag](){return"FSharpSet"}toJSON(){return Array.from(this)}CompareTo(e){return 0|SetTreeModule_compare(FSharpSet__get_Comparer(this),FSharpSet__get_Tree(this),FSharpSet__get_Tree(e))}"System.Collections.Generic.ICollection`1.Add2B595"(e){throw new Error("ReadOnlyCollection")}"System.Collections.Generic.ICollection`1.Clear"(){throw new Error("ReadOnlyCollection")}"System.Collections.Generic.ICollection`1.Remove2B595"(e){throw new Error("ReadOnlyCollection")}"System.Collections.Generic.ICollection`1.Contains2B595"(e){return SetTreeModule_mem(FSharpSet__get_Comparer(this),e,FSharpSet__get_Tree(this))}"System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(e,t){SetTreeModule_copyToArray(FSharpSet__get_Tree(this),e,t)}"System.Collections.Generic.ICollection`1.get_IsReadOnly"(){return!0}"System.Collections.Generic.ICollection`1.get_Count"(){return 0|FSharpSet__get_Count(this)}"System.Collections.Generic.IReadOnlyCollection`1.get_Count"(){return 0|FSharpSet__get_Count(this)}GetEnumerator(){return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(this))}[Symbol.iterator](){return toIterator(this.GetEnumerator())}"System.Collections.IEnumerable.GetEnumerator"(){return SetTreeModule_mkIEnumerator(FSharpSet__get_Tree(this))}get size(){return 0|FSharpSet__get_Count(this)}add(e){throw new Error("Set cannot be mutated")}clear(){throw new Error("Set cannot be mutated")}delete(e){throw new Error("Set cannot be mutated")}has(e){return FSharpSet__Contains(this,e)}keys(){return map_1((e=>e),this)}values(){return map_1((e=>e),this)}entries(){return map_1((e=>[e,e]),this)}forEach(e,t){const r=this;iterate_1((t=>{partialApply(2,e,[t])(t)(r)}),r)}}export function FSharpSet$reflection(e){return class_type("Set.FSharpSet",[e],FSharpSet)}export function FSharpSet_$ctor(e,t){return new FSharpSet(e,t)}export function FSharpSet__get_Comparer(e){return e.comparer}export function FSharpSet__get_Tree(e){return e.tree}export function FSharpSet_Empty(e){return FSharpSet_$ctor(e,SetTreeModule_empty())}export function FSharpSet__Add(e,t){return FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_add(FSharpSet__get_Comparer(e),t,FSharpSet__get_Tree(e)))}export function FSharpSet__Remove(e,t){return FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_remove(FSharpSet__get_Comparer(e),t,FSharpSet__get_Tree(e)))}export function FSharpSet__get_Count(e){return SetTreeModule_count(FSharpSet__get_Tree(e))}export function FSharpSet__Contains(e,t){return SetTreeModule_mem(FSharpSet__get_Comparer(e),t,FSharpSet__get_Tree(e))}export function FSharpSet__Iterate(e,t){SetTreeModule_iter(t,FSharpSet__get_Tree(e))}export function FSharpSet__Fold(e,t,r){const o=t;return SetTreeModule_fold(((e,t)=>o(t,e)),r,FSharpSet__get_Tree(e))}export function FSharpSet__get_IsEmpty(e){return null==FSharpSet__get_Tree(e)}export function FSharpSet__Partition(e,t){if(null==FSharpSet__get_Tree(e))return[e,e];{const r=SetTreeModule_partition(FSharpSet__get_Comparer(e),t,FSharpSet__get_Tree(e));return[FSharpSet_$ctor(FSharpSet__get_Comparer(e),r[0]),FSharpSet_$ctor(FSharpSet__get_Comparer(e),r[1])]}}export function FSharpSet__Filter(e,t){return null==FSharpSet__get_Tree(e)?e:FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_filter(FSharpSet__get_Comparer(e),t,FSharpSet__get_Tree(e)))}export function FSharpSet__Map(e,t,r){return FSharpSet_$ctor(r,SetTreeModule_fold(((e,o)=>SetTreeModule_add(r,t(o),e)),SetTreeModule_empty(),FSharpSet__get_Tree(e)))}export function FSharpSet__Exists(e,t){return SetTreeModule_exists(t,FSharpSet__get_Tree(e))}export function FSharpSet__ForAll(e,t){return SetTreeModule_forall(t,FSharpSet__get_Tree(e))}export function FSharpSet_op_Subtraction(e,t){return null==FSharpSet__get_Tree(e)||null==FSharpSet__get_Tree(t)?e:FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_diff(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t)))}export function FSharpSet_op_Addition(e,t){return null==FSharpSet__get_Tree(t)?e:null==FSharpSet__get_Tree(e)?t:FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_union(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t)))}export function FSharpSet_Intersection(e,t){return null==FSharpSet__get_Tree(t)?t:null==FSharpSet__get_Tree(e)?e:FSharpSet_$ctor(FSharpSet__get_Comparer(e),SetTreeModule_intersection(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t)))}export function FSharpSet_IntersectionMany(e){return reduce(FSharpSet_Intersection,e)}export function FSharpSet_Equality(e,t){return 0===SetTreeModule_compare(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function FSharpSet_Compare(e,t){return SetTreeModule_compare(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function FSharpSet__get_Choose(e){return SetTreeModule_choose(FSharpSet__get_Tree(e))}export function FSharpSet__get_MinimumElement(e){return SetTreeModule_minimumElement(FSharpSet__get_Tree(e))}export function FSharpSet__get_MaximumElement(e){return SetTreeModule_maximumElement(FSharpSet__get_Tree(e))}export function FSharpSet__IsSubsetOf(e,t){return SetTreeModule_subset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function FSharpSet__IsSupersetOf(e,t){return SetTreeModule_subset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(t),FSharpSet__get_Tree(e))}export function FSharpSet__IsProperSubsetOf(e,t){return SetTreeModule_properSubset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function FSharpSet__IsProperSupersetOf(e,t){return SetTreeModule_properSubset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(t),FSharpSet__get_Tree(e))}export function FSharpSet__ToList(e){return SetTreeModule_toList(FSharpSet__get_Tree(e))}export function FSharpSet__ToArray(e){return SetTreeModule_toArray(FSharpSet__get_Tree(e))}export function FSharpSet__ComputeHashCode(e){let t=0;const r=getEnumerator(e);try{for(;r["System.Collections.IEnumerator.MoveNext"]();){const e=r["System.Collections.Generic.IEnumerator`1.get_Current"]();t=(t<<1)+structuralHash(e)+631|0}}finally{disposeSafe(r)}return 0|Math.abs(t)}export function isEmpty(e){return FSharpSet__get_IsEmpty(e)}export function contains(e,t){return FSharpSet__Contains(t,e)}export function add(e,t){return FSharpSet__Add(t,e)}export function singleton(e,t){return FSharpSet__Add(FSharpSet_Empty(t),e)}export function remove(e,t){return FSharpSet__Remove(t,e)}export function union(e,t){return FSharpSet_op_Addition(e,t)}export function unionMany(e,t){return fold_3(FSharpSet_op_Addition,FSharpSet_Empty(t),e)}export function intersect(e,t){return FSharpSet_Intersection(e,t)}export function intersectMany(e){return FSharpSet_IntersectionMany(e)}export function iterate(e,t){FSharpSet__Iterate(t,e)}export function empty(e){return FSharpSet_Empty(e)}export function forAll(e,t){return FSharpSet__ForAll(t,e)}export function exists(e,t){return FSharpSet__Exists(t,e)}export function filter(e,t){return FSharpSet__Filter(t,e)}export function partition(e,t){return FSharpSet__Partition(t,e)}export function fold(e,t,r){return SetTreeModule_fold(e,t,FSharpSet__get_Tree(r))}export function foldBack(e,t,r){return SetTreeModule_foldBack(e,FSharpSet__get_Tree(t),r)}export function map(e,t,r){return FSharpSet__Map(t,e,r)}export function count(e){return FSharpSet__get_Count(e)}export function ofList(e,t){return FSharpSet_$ctor(t,SetTreeModule_ofSeq(t,e))}export function ofArray(e,t){return FSharpSet_$ctor(t,SetTreeModule_ofArray(t,e))}export function toList(e){return FSharpSet__ToList(e)}export function toArray(e){return FSharpSet__ToArray(e)}export function toSeq(e){return map_1((e=>e),e)}export function ofSeq(e,t){return FSharpSet_$ctor(t,SetTreeModule_ofSeq(t,e))}export function difference(e,t){return FSharpSet_op_Subtraction(e,t)}export function isSubset(e,t){return SetTreeModule_subset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function isSuperset(e,t){return SetTreeModule_subset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(t),FSharpSet__get_Tree(e))}export function isProperSubset(e,t){return SetTreeModule_properSubset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(e),FSharpSet__get_Tree(t))}export function isProperSuperset(e,t){return SetTreeModule_properSubset(FSharpSet__get_Comparer(e),FSharpSet__get_Tree(t),FSharpSet__get_Tree(e))}export function minElement(e){return FSharpSet__get_MinimumElement(e)}export function maxElement(e){return FSharpSet__get_MaximumElement(e)}export function unionWith(e,t){return fold_3(((e,t)=>e.add(t)),e,t)}export function newMutableSetWith(e,t){return e instanceof HashSet?HashSet_$ctor_Z6150332D(t,HashSet__get_Comparer(e)):new Set(t)}export function intersectWith(e,t){const r=newMutableSetWith(e,t);iterate_1((t=>{r.has(t)||e.delete(t)}),e.values())}export function exceptWith(e,t){iterate_1((t=>{e.delete(t)}),t)}export function isSubsetOf(e,t){const r=newMutableSetWith(e,t);return forAll_1((e=>r.has(e)),e.values())}export function isSupersetOf(e,t){return forAll_1((t=>e.has(t)),t)}export function isProperSubsetOf(e,t){const r=newMutableSetWith(e,t);return r.size>e.size&&forAll_1((e=>r.has(e)),e.values())}export function isProperSupersetOf(e,t){const r=cache(t);return!!exists_1((t=>!e.has(t)),r)&&forAll_1((t=>e.has(t)),r)}