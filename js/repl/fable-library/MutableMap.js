import{disposeSafe,defaultOf,partialApply,equals,toIterator,getEnumerator}from"./Util.js";import{iterate,map,delay,toArray,iterateIndexed,concat}from"./Seq.js";import{FSharpRef}from"./Types.js";import{class_type}from"./Reflection.js";import{getItemFromDict,tryGetValue}from"./MapUtil.js";import{format}from"./String.js";export class Dictionary{constructor(t,e){const r=new FSharpRef(defaultOf());this.comparer=e,r.contents=this,this.hashMap=new Map([]),this["init@9"]=1;const n=getEnumerator(t);try{for(;n["System.Collections.IEnumerator.MoveNext"]();){const t=n["System.Collections.Generic.IEnumerator`1.get_Current"]();Dictionary__Add_5BDDA1(r.contents,t[0],t[1])}}finally{disposeSafe(n)}}get[Symbol.toStringTag](){return"Dictionary"}toJSON(){return Array.from(this)}"System.Collections.IEnumerable.GetEnumerator"(){return getEnumerator(this)}GetEnumerator(){return getEnumerator(concat(this.hashMap.values()))}[Symbol.iterator](){return toIterator(this.GetEnumerator())}"System.Collections.Generic.ICollection`1.Add2B595"(t){Dictionary__Add_5BDDA1(this,t[0],t[1])}"System.Collections.Generic.ICollection`1.Clear"(){Dictionary__Clear(this)}"System.Collections.Generic.ICollection`1.Contains2B595"(t){const e=Dictionary__TryFind_2B595(this,t[0]);let r;switch(r=null!=e&&equals(e[1],t[1])?0:1,r){case 0:return!0;case 1:return!1}}"System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(t,e){iterateIndexed(((r,n)=>{t[e+r]=n}),this)}"System.Collections.Generic.ICollection`1.get_Count"(){return 0|Dictionary__get_Count(this)}"System.Collections.Generic.ICollection`1.get_IsReadOnly"(){return!1}"System.Collections.Generic.ICollection`1.Remove2B595"(t){const e=Dictionary__TryFind_2B595(this,t[0]);return null!=e&&(equals(e[1],t[1])&&Dictionary__Remove_2B595(this,t[0]),!0)}"System.Collections.Generic.IDictionary`2.Add5BDDA1"(t,e){Dictionary__Add_5BDDA1(this,t,e)}"System.Collections.Generic.IDictionary`2.ContainsKey2B595"(t){return Dictionary__ContainsKey_2B595(this,t)}"System.Collections.Generic.IDictionary`2.get_Item2B595"(t){return Dictionary__get_Item_2B595(this,t)}"System.Collections.Generic.IDictionary`2.set_Item5BDDA1"(t,e){Dictionary__set_Item_5BDDA1(this,t,e)}"System.Collections.Generic.IDictionary`2.get_Keys"(){const t=this;return toArray(delay((()=>map((t=>t[0]),t))))}"System.Collections.Generic.IDictionary`2.Remove2B595"(t){return Dictionary__Remove_2B595(this,t)}"System.Collections.Generic.IDictionary`2.TryGetValue6DC89625"(t,e){const r=Dictionary__TryFind_2B595(this,t);if(null!=r){const t=r;return e.contents=t[1],!0}return!1}"System.Collections.Generic.IDictionary`2.get_Values"(){const t=this;return toArray(delay((()=>map((t=>t[1]),t))))}get size(){return 0|Dictionary__get_Count(this)}clear(){Dictionary__Clear(this)}delete(t){return Dictionary__Remove_2B595(this,t)}entries(){return map((t=>[t[0],t[1]]),this)}get(t){return Dictionary__get_Item_2B595(this,t)}has(t){return Dictionary__ContainsKey_2B595(this,t)}keys(){return map((t=>t[0]),this)}set(t,e){return Dictionary__set_Item_5BDDA1(this,t,e),this}values(){return map((t=>t[1]),this)}forEach(t,e){const r=this;iterate((e=>{partialApply(2,t,[e[1]])(e[0])(r)}),r)}}export function Dictionary$reflection(t,e){return class_type("Fable.Collections.Dictionary",[t,e],Dictionary)}export function Dictionary_$ctor_6623D9B3(t,e){return new Dictionary(t,e)}function Dictionary__TryFindIndex_2B595(t,e){const r=0|t.comparer.GetHashCode(e);let n,i=defaultOf();return n=[tryGetValue(t.hashMap,r,new FSharpRef((()=>i),(t=>{i=t}))),i],n[0]?[!0,r,n[1].findIndex((r=>t.comparer.Equals(e,r[0])))]:[!1,r,-1]}export function Dictionary__TryFind_2B595(t,e){const r=Dictionary__TryFindIndex_2B595(t,e);let n;switch(n=r[0]&&r[2]>-1?0:1,n){case 0:return getItemFromDict(t.hashMap,r[1])[r[2]];case 1:return}}export function Dictionary__get_Comparer(t){return t.comparer}export function Dictionary__Clear(t){t.hashMap.clear()}export function Dictionary__get_Count(t){let e=0,r=getEnumerator(t.hashMap.values());try{for(;r["System.Collections.IEnumerator.MoveNext"]();)e=e+r["System.Collections.Generic.IEnumerator`1.get_Current"]().length|0}finally{disposeSafe(r)}return 0|e}export function Dictionary__get_Item_2B595(t,e){const r=Dictionary__TryFind_2B595(t,e);if(null!=r)return r[1];throw new Error("The item was not found in collection")}export function Dictionary__set_Item_5BDDA1(t,e,r){const n=Dictionary__TryFindIndex_2B595(t,e);n[0]?n[2]>-1?getItemFromDict(t.hashMap,n[1])[n[2]]=[e,r]:getItemFromDict(t.hashMap,n[1]).push([e,r]):t.hashMap.set(n[1],[[e,r]])}export function Dictionary__Add_5BDDA1(t,e,r){const n=Dictionary__TryFindIndex_2B595(t,e);if(n[0]){if(n[2]>-1)throw new Error(format("An item with the same key has already been added. Key: {0}",e));getItemFromDict(t.hashMap,n[1]).push([e,r])}else t.hashMap.set(n[1],[[e,r]])}export function Dictionary__ContainsKey_2B595(t,e){const r=Dictionary__TryFindIndex_2B595(t,e);let n;switch(n=r[0]&&r[2]>-1?0:1,n){case 0:return!0;case 1:return!1}}export function Dictionary__Remove_2B595(t,e){const r=Dictionary__TryFindIndex_2B595(t,e);let n;switch(n=r[0]&&r[2]>-1?0:1,n){case 0:return getItemFromDict(t.hashMap,r[1]).splice(r[2],1),!0;case 1:return!1}}