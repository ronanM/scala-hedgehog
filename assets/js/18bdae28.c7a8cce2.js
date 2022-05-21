"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[376],{3905:function(e,t,n){n.d(t,{Zo:function(){return c},kt:function(){return d}});var r=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function a(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,i=function(e,t){if(null==e)return{};var n,r,i={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var l=r.createContext({}),u=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):a(a({},t),e)),n},c=function(e){var t=u(e.components);return r.createElement(l.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},g=r.forwardRef((function(e,t){var n=e.components,i=e.mdxType,o=e.originalType,l=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),g=u(n),d=i,m=g["".concat(l,".").concat(d)]||g[d]||p[d]||o;return n?r.createElement(m,a(a({ref:t},c),{},{components:n})):r.createElement(m,a({ref:t},c))}));function d(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var o=n.length,a=new Array(o);a[0]=g;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s.mdxType="string"==typeof e?e:i,a[1]=s;for(var u=2;u<o;u++)a[u]=n[u];return r.createElement.apply(null,a)}return r.createElement.apply(null,n)}g.displayName="MDXCreateElement"},5235:function(e,t,n){n.r(t),n.d(t,{assets:function(){return c},contentTitle:function(){return l},default:function(){return d},frontMatter:function(){return s},metadata:function(){return u},toc:function(){return p}});var r=n(7462),i=n(3366),o=(n(7294),n(3905)),a=["components"],s={title:"Integration with other test libraries",sidebar_label:"MUnit",slug:"/integration-munit"},l=void 0,u={unversionedId:"integration/munit",id:"integration/munit",title:"Integration with other test libraries",description:"Integration with other test libraries",source:"@site/../generated-docs/target/mdoc/integration/munit.md",sourceDirName:"integration",slug:"/integration-munit",permalink:"/scala-hedgehog/docs/integration-munit",draft:!1,tags:[],version:"current",frontMatter:{title:"Integration with other test libraries",sidebar_label:"MUnit",slug:"/integration-munit"},sidebar:"docs",previous:{title:"Minitest",permalink:"/scala-hedgehog/docs/integration-minitest"}},c={},p=[{value:"Integration with other test libraries",id:"integration-with-other-test-libraries",level:2},{value:"munit",id:"munit",level:3}],g={toc:p};function d(e){var t=e.components,n=(0,i.Z)(e,a);return(0,o.kt)("wrapper",(0,r.Z)({},g,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h2",{id:"integration-with-other-test-libraries"},"Integration with other test libraries"),(0,o.kt)("h3",{id:"munit"},"munit"),(0,o.kt)("p",null,"Scala Hedgehog provides an integration module for ",(0,o.kt)("a",{parentName:"p",href:"https://scalameta.org/munit/"},"munit"),". This allows you to define property-based and example-based Hedgehog tests within a munit test suite. If you use this integration, you won't need to Scala Hedgehog sbt testing extension, because you're using the one provided by munit:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'val hedgehogVersion = "0.9.0"\nlibraryDependencies += "qa.hedgehog" %% "hedgehog-munit" % hedgehogVersion\n\ntestFrameworks += TestFramework("munit.runner.Framework")\n')),(0,o.kt)("p",null,"Here's an example of using ",(0,o.kt)("inlineCode",{parentName:"p"},"hedgehog-munit"),":"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import hedgehog.munit.HedgehogSuite\nimport hedgehog._\n\nclass ReverseSuite extends HedgehogSuite {\n  property("reverse alphabetic strings") {\n    for {\n      xs <- Gen.alpha.list(Range.linear(0, 100)).forAll\n    } yield assertEquals(xs.reverse.reverse, xs)\n  }\n  \n  test("reverse hello") {\n    withMunitAssertions{ assertions =>\n      asertions.assertEqual("hello".reverse, "olleh")\n    }\n    "hello".reverse ==== "olleh"\n  }\n}\n')),(0,o.kt)("p",null,"HedgehogSuite provides ",(0,o.kt)("inlineCode",{parentName:"p"},"munit"),"-like assertions, along with all the ",(0,o.kt)("inlineCode",{parentName:"p"},"hedgehog.Result")," methods and members, that return results in the standard hedgehog report format while satisfying munit's exception-based test failures."))}d.isMDXComponent=!0}}]);