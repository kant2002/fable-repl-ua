export function create(e,t=0){if(0!=(-284&t))throw new Error("RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline and ECMAScript");let n="gu";return n+=1&t?"i":"",n+=2&t?"m":"",n+=16&t?"s":"",new RegExp(e,n)}export function escape(e){return e.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g,"\\$&")}export function unescape(e){return e.replace(/\\([\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|])/g,"$1")}export function isMatch(e,t,n=0){return e.lastIndex=n,e.test(t)}export function match(e,t,n=0){return e.lastIndex=n,e.exec(t)}export function matches(e,t,n=0){if(null==t)throw new Error("Input cannot ve null");if(!e.global)throw new Error("Non-global RegExp");e.lastIndex=n;const r=[];let o,i=-1;for(;null!=(o=e.exec(t));)o.index===i?e.lastIndex++:(i=o.index,r.push(o));return r}export function options(e){let t=256;return t|=e.ignoreCase?1:0,t|=e.multiline?2:0,t}export function replace(e,t,n,r,o=0){if("string"==typeof e){const n=e;e=create(t,null!=r?r:0),t=n,r=void 0}if("function"==typeof n)return r=null==r?-1:r,t.substring(0,o)+t.substring(o).replace(e,(function(){let e=arguments[0];if(r){r--;const t=[],o=arguments.length,i="string"!=typeof arguments[o-1];let l=i?o-3:o-2;for(let e=0;e<l;e++)t.push(arguments[e]);t.index=arguments[l++],t.input=arguments[l++],i&&(t.groups=arguments[l]),e=n(t)}return e}));if(n=n.replace(/\$0/g,(e=>"$&")).replace(/\${([^}]+)}/g,"$<$1>"),null!=r){let i;const l=t.substring(o),u=matches(e,l),s=matches.length>r?(i=u[r-1],l.substring(0,i.index+i[0].length)):l;return t.substring(0,o)+s.replace(e,n)+t.substring(o+s.length)}return t.replace(e,n)}export function split(e,t,n,r=0){if("string"==typeof e){const r=e;e=create(t,null!=n?n:0),t=r,n=void 0}return(t=t.substring(r)).split(e,n)}