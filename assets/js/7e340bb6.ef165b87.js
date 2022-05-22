"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[255],{3905:function(e,n,a){a.d(n,{Zo:function(){return p},kt:function(){return u}});var t=a(7294);function r(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function i(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function l(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?i(Object(a),!0).forEach((function(n){r(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function o(e,n){if(null==e)return{};var a,t,r=function(e,n){if(null==e)return{};var a,t,r={},i=Object.keys(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||(r[a]=e[a]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var c=t.createContext({}),s=function(e){var n=t.useContext(c),a=n;return e&&(a="function"==typeof e?e(n):l(l({},n),e)),a},p=function(e){var n=s(e.components);return t.createElement(c.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},g=t.forwardRef((function(e,n){var a=e.components,r=e.mdxType,i=e.originalType,c=e.parentName,p=o(e,["components","mdxType","originalType","parentName"]),g=s(a),u=r,m=g["".concat(c,".").concat(u)]||g[u]||d[u]||i;return a?t.createElement(m,l(l({ref:n},p),{},{components:a})):t.createElement(m,l({ref:n},p))}));function u(e,n){var a=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=a.length,l=new Array(i);l[0]=g;var o={};for(var c in n)hasOwnProperty.call(n,c)&&(o[c]=n[c]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var s=2;s<i;s++)l[s]=a[s];return t.createElement.apply(null,l)}return t.createElement.apply(null,a)}g.displayName="MDXCreateElement"},9937:function(e,n,a){a.r(n),a.d(n,{assets:function(){return p},contentTitle:function(){return c},default:function(){return u},frontMatter:function(){return o},metadata:function(){return s},toc:function(){return d}});var t=a(7462),r=a(3366),i=(a(7294),a(3905)),l=["components"],o={title:"Migration From ScalaCheck",sidebar_position:4,sidebar_label:"Migration From ScalaCheck",slug:"/guides-migration-from-scalacheck"},c=void 0,s={unversionedId:"guides/migration-scalacheck",id:"guides/migration-scalacheck",title:"Migration From ScalaCheck",description:"Migration From ScalaCheck",source:"@site/../generated-docs/target/mdoc/guides/migration-scalacheck.md",sourceDirName:"guides",slug:"/guides-migration-from-scalacheck",permalink:"/scala-hedgehog/docs/guides-migration-from-scalacheck",draft:!1,tags:[],version:"current",sidebarPosition:4,frontMatter:{title:"Migration From ScalaCheck",sidebar_position:4,sidebar_label:"Migration From ScalaCheck",slug:"/guides-migration-from-scalacheck"},sidebar:"tutorialSidebar",previous:{title:"State-Based Testing (2)",permalink:"/scala-hedgehog/docs/guides-state-tutorial-vars"},next:{title:"Differences to Haskell Hedgehog",permalink:"/scala-hedgehog/docs/guides-haskell-differences"}},p={},d=[{value:"Migration From ScalaCheck",id:"migration-from-scalacheck",level:2},{value:"Properties",id:"properties",level:2},{value:"ScalaCheck",id:"scalacheck",level:3},{value:"Hedgehog",id:"hedgehog",level:3},{value:"Gen",id:"gen",level:2},{value:"ScalaCheck",id:"scalacheck-1",level:3},{value:"Hedgehog",id:"hedgehog-1",level:3},{value:"Arbitrary",id:"arbitrary",level:2},{value:"ScalaCheck",id:"scalacheck-2",level:3},{value:"Shrink",id:"shrink",level:2},{value:"ScalaCheck",id:"scalacheck-3",level:3},{value:"Hedgehog",id:"hedgehog-2",level:3}],g={toc:d};function u(e){var n=e.components,a=(0,r.Z)(e,l);return(0,i.kt)("wrapper",(0,t.Z)({},g,a,{components:n,mdxType:"MDXLayout"}),(0,i.kt)("h2",{id:"migration-from-scalacheck"},"Migration From ScalaCheck"),(0,i.kt)("p",null,"For many cases migrating from ScalaCheck to Hedgehog should be ",(0,i.kt)("em",{parentName:"p"},"fairly"),"\nstraight forward, as the general principals are quite similar, and the changes\nare largely syntactic."),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("a",{parentName:"li",href:"#properties"},"Properties")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("a",{parentName:"li",href:"#arbitary"},"Arbitrary")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("a",{parentName:"li",href:"#gen"},"Gen"))),(0,i.kt)("h2",{id:"properties"},"Properties"),(0,i.kt)("p",null,"Some basic rules:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Replace ",(0,i.kt)("inlineCode",{parentName:"p"},'Properties("...")')," with just ",(0,i.kt)("inlineCode",{parentName:"p"},"Properties"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Replace ",(0,i.kt)("inlineCode",{parentName:"p"},"Prop.forAll")," with a call to ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/hedgehogqa/scala-hedgehog/search?q=%22def+forAll%22&unscoped_q=%22def+forAll%22"},"forAll")," on a specific ",(0,i.kt)("inlineCode",{parentName:"p"},"Gen")," instance"),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"If you have previously been relying on ",(0,i.kt)("inlineCode",{parentName:"p"},"Arbitrary")," instances these ",(0,i.kt)("em",{parentName:"p"},"have"),"\nto be replaced with calls to functions that return an instance of ",(0,i.kt)("inlineCode",{parentName:"p"},"Gen"),"."),(0,i.kt)("p",{parentName:"li"},"See the ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/hedgehogqa/scala-hedgehog/tree/master/core/src/main/scala/hedgehog/extra"},"extra")," package for some stand Scala data type combinators."),(0,i.kt)("p",{parentName:"li"},"For more information see the section on ",(0,i.kt)("a",{parentName:"p",href:"#gen"},"Gen"),".")))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},(0,i.kt)("inlineCode",{parentName:"p"},"flatMap")," over the result of your ",(0,i.kt)("inlineCode",{parentName:"p"},"genFoo.forAll"),", or use a ",(0,i.kt)("inlineCode",{parentName:"p"},"for"),"\ncomprehension.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Return your ",(0,i.kt)("inlineCode",{parentName:"p"},"Prop")," or ",(0,i.kt)("inlineCode",{parentName:"p"},"Boolean")," assetions with ",(0,i.kt)("inlineCode",{parentName:"p"},"Result.assert(...)"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Replace ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/typelevel/scalacheck/search?q=%22def+label%22&unscoped_q=%22def+label%22"},"label")," or ",(0,i.kt)("inlineCode",{parentName:"p"},":|")," with  ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/hedgehogqa/scala-hedgehog/search?q=%22def+log%22&unscoped_q=%22def+log%22"},"Result.log(...)"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Replace equality assertions like ",(0,i.kt)("inlineCode",{parentName:"p"},"?=")," with ",(0,i.kt)("inlineCode",{parentName:"p"},"====")))),(0,i.kt)("h3",{id:"scalacheck"},"ScalaCheck"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'import org.scalacheck._\n\nobject StringSpecification extends Properties("String") {\n\n  property("startsWith") =\n    Prop.forAll { (a: String, b: String) =>\n      (a+b).startsWith(a)\n    }\n}\n')),(0,i.kt)("h3",{id:"hedgehog"},"Hedgehog"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'import hedgehog._\nimport hedgehog.runner._\n\nobject StringSpecification extends Properties {\n\n  override def tests: List[Test] = List(\n    property("startsWith", for {\n      a <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll\n      b <- Gen.string(Gen.unicode, Range.linear(0, 100)).forAll\n      } yield Result.assert((a+b).startsWith(a))\n    )\n  )\n}\n')),(0,i.kt)("h2",{id:"gen"},"Gen"),(0,i.kt)("p",null,"Some basic rules:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"Gen.list")," and ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen.listOfN")," can be replaced with a call to\n",(0,i.kt)("inlineCode",{parentName:"li"},"list(Range.linear(0, n))")," on a specific ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen")," instance."),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"Gen.const")," is now ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen.constant")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"Arbitrary.arbitrary[Int]")," is now ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen.int(Range.linear(min, max))")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"Gen.oneOf")," is now ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen.choice1"))),(0,i.kt)("p",null,'It\'s important to note that there are no more "default" ',(0,i.kt)("inlineCode",{parentName:"p"},"Arbitrary")," instances\nto summon. You ",(0,i.kt)("em",{parentName:"p"},"must")," decided what kind of ",(0,i.kt)("inlineCode",{parentName:"p"},"int")," or ",(0,i.kt)("inlineCode",{parentName:"p"},"String")," you want to\ngenerate, and what their ",(0,i.kt)("inlineCode",{parentName:"p"},"Range")," is."),(0,i.kt)("h3",{id:"scalacheck-1"},"ScalaCheck"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"val genLeaf = Gen.const(Leaf)\n\nval genNode = for {\n  v <- arbitrary[Int]\n  left <- genTree\n  right <- genTree\n} yield Node(left, right, v)\n\ndef genTree: Gen[Tree] = Gen.oneOf(genLeaf, genNode)\n")),(0,i.kt)("h3",{id:"hedgehog-1"},"Hedgehog"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"val genLeaf = Gen.constant(Leaf)\n\nval genNode = for {\n  v <- Gen.int(Range.linear(Integer.MaxValue, Integer.MinValue))\n  left <- genTree\n  right <- genTree\n} yield Node(left, right, v)\n\ndef genTree: Gen[Tree] = Gen.choice1(genLeaf, genNode)\n")),(0,i.kt)("h2",{id:"arbitrary"},"Arbitrary"),(0,i.kt)("p",null,"Some basic rules:"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},"Replace ",(0,i.kt)("inlineCode",{parentName:"li"},"implict def")," functions that return ",(0,i.kt)("inlineCode",{parentName:"li"},"Arbitrary")," to a function\nthat returns the ",(0,i.kt)("inlineCode",{parentName:"li"},"Gen")," directly.")),(0,i.kt)("h3",{id:"scalacheck-2"},"ScalaCheck"),(0,i.kt)("p",null,"This example was taken from the ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md#the-arbitrary-generator"},"ScalaCheck Guide"),"."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] = Arbitrary {\n\n  val genLeaf = for(e <- Arbitrary.arbitrary[T]) yield Leaf(e)\n\n  def genInternal(sz: Int): Gen[Tree[T]] = for {\n    n <- Gen.choose(sz/3, sz/2)\n    c <- Gen.listOfN(n, sizedTree(sz/2))\n  } yield Internal(c)\n\n  def sizedTree(sz: Int) =\n    if(sz <= 0) genLeaf\n    else Gen.frequency((1, genLeaf), (3, genInternal(sz)))\n\n  Gen.sized(sz => sizedTree(sz))\n}\n")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"def genTree[T](g: Gen[T]): Gen[Tree[T]] = {\n\n  val genLeaf = for(e <- g) yield Leaf(e)\n\n  def genInternal(sz: Size): Gen[Tree[T]] = for {\n    n <- Gen.choose(sz.value/3, sz.value/2)\n    c <- sizedTree(sz.value/2).list(Range.linear(0, n))\n  } yield Internal(c)\n\n  def sizedTree(sz: Size) =\n    if(sz.value <= 0) genLeaf\n    else Gen.frequency1((1, genLeaf), (3, genInternal(sz)))\n\n  Gen.sized(sz => sizedTree(sz))\n}\n")),(0,i.kt)("h2",{id:"shrink"},"Shrink"),(0,i.kt)("p",null,"This is assuming you're even writing them in the first place..."),(0,i.kt)("h3",{id:"scalacheck-3"},"ScalaCheck"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"case class Data(a: String, i: Int)\n\nimplicit def arbData: Arbitrary[Data] =\n  Arbitrary[Data] {\n    for {\n      s <- arbitrary[String]\n      i <- arbitrary[Int]\n    } yield Data(a, i)\n  }\n\nimplicit def shrink: Shrink[Data] =\n  Shrink[Data] { case Data(a, i) =>\n    shrink(a).map(a2 => Data(a2, i)) append\n    shrink(i).map(i2 => Data(a, i2))\n  }\n")),(0,i.kt)("h3",{id:"hedgehog-2"},"Hedgehog"),(0,i.kt)("p",null,"Good news, you don't need to do anything! Just write your generators."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"def genData: Gen[Data] =\n  for {\n    s <- Gen.string(Gen.unicode, Range.linear(0, 100))\n    i <- Gen.int(Range.linear(-100, 100))\n  } yield Data(a, i)\n")))}u.isMDXComponent=!0}}]);