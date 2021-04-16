(function(e){function t(t){for(var r,s,i=t[0],a=t[1],c=t[2],p=0,d=[];p<i.length;p++)s=i[p],Object.prototype.hasOwnProperty.call(o,s)&&o[s]&&d.push(o[s][0]),o[s]=0;for(r in a)Object.prototype.hasOwnProperty.call(a,r)&&(e[r]=a[r]);u&&u(t);while(d.length)d.shift()();return l.push.apply(l,c||[]),n()}function n(){for(var e,t=0;t<l.length;t++){for(var n=l[t],r=!0,i=1;i<n.length;i++){var a=n[i];0!==o[a]&&(r=!1)}r&&(l.splice(t--,1),e=s(s.s=n[0]))}return e}var r={},o={app:0},l=[];function s(t){if(r[t])return r[t].exports;var n=r[t]={i:t,l:!1,exports:{}};return e[t].call(n.exports,n,n.exports,s),n.l=!0,n.exports}s.m=e,s.c=r,s.d=function(e,t,n){s.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:n})},s.r=function(e){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},s.t=function(e,t){if(1&t&&(e=s(e)),8&t)return e;if(4&t&&"object"===typeof e&&e&&e.__esModule)return e;var n=Object.create(null);if(s.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var r in e)s.d(n,r,function(t){return e[t]}.bind(null,r));return n},s.n=function(e){var t=e&&e.__esModule?function(){return e["default"]}:function(){return e};return s.d(t,"a",t),t},s.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},s.p="/";var i=window["webpackJsonp"]=window["webpackJsonp"]||[],a=i.push.bind(i);i.push=t,i=i.slice();for(var c=0;c<i.length;c++)t(i[c]);var u=a;l.push([0,"chunk-vendors"]),n()})({0:function(e,t,n){e.exports=n("56d7")},3503:function(e,t,n){"use strict";n("80b5")},"47f1":function(e,t,n){},"56d7":function(e,t,n){"use strict";n.r(t);n("e260"),n("e6cf"),n("cca6"),n("a79d");var r=n("7a23");function o(e,t,n,o,l,s){var i=Object(r["w"])("requests");return Object(r["o"])(),Object(r["f"])(i)}n("a4d3"),n("e01a");var l=n("610a"),s=n.n(l),i=Object(r["G"])("data-v-84e92d88");Object(r["r"])("data-v-84e92d88");var a={class:"container mb-14 p-8"},c=Object(r["i"])("a",{href:"/"},[Object(r["i"])("img",{alt:"bees",src:s.a,style:{opacity:"60%",width:"27%",display:"block","margin-left":"auto","margin-right":"auto"}})],-1),u=Object(r["i"])("p",{class:"mt-8 font-medium font-sans text-2xl"},"Poll'em",-1),p=Object(r["i"])("p",{class:"font-sans mb-8"},[Object(r["h"])(" A simple & hassle-free poll application "),Object(r["i"])("span",{class:"font-thin"}," with a Haskell backend.")],-1),d={key:0,class:"m-4"},b={key:1,class:"m-4"},h=Object(r["i"])("p",null," You do not appear to be authenticated on this app. This means you can only view already existing polls. How about you authenticate so that you can start creating polls? ",-1),f={class:"m-8"},g={key:1,class:"disabled:opacity-50 ml-2 p-1 bg-green-500 text-white font-semibold rounded-lg shadow-md",disabled:""},y={key:0,class:"mt-4"},m={key:1,class:"disabled:opacity-50 p-1 bg-green-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75",disabled:""},O={key:0,class:"mt-10"},_={id:"displayed",class:"flex flex-col"},j=Object(r["i"])("label",{for:"takingMultiple"},"Allowed to select multiple answers?",-1),k=Object(r["i"])("label",{for:"takingVisible"},"Results visible during poll?",-1),v={class:"space-x-3"},w=Object(r["i"])("label",{for:"takingStartDate"},"Poll started at:",-1),x={key:0},P=Object(r["i"])("label",{class:"space-y-3",for:"takingEndDate"},"(optional) Poll due to close at:",-1),D={key:1},S=Object(r["i"])("p",null,"Nothing to render.",-1),q={id:"creatingPoll",class:"flex flex-col"},A={class:"mt-5"},C=Object(r["i"])("label",{for:"creatingMultiple"},"Allow to select multiple answers",-1),$=Object(r["i"])("label",{for:"creatingVisible"},"Make results visible during poll",-1),T={class:"mt-3 space-y-2"},R=Object(r["i"])("label",{for:"creatingStartDate"},"Poll should start at:",-1),V=Object(r["i"])("br",null,null,-1),M=Object(r["i"])("label",{for:"creatingEndDate"},"(optional) Poll should end at:",-1),I={key:0},E=Object(r["i"])("label",null,"created",-1),U=Object(r["i"])("label",null,"taken:",-1),N={class:"mt-5"},B=Object(r["i"])("p",{class:"text-sm font-light"}," You've taken or created polls you think should be displayed here? You can retrieve your entire history. Please be considerate in your use of this feature, as the database is not optimized for this (expect serious debounce). Also your last created poll might be missing if you created it very recently. ",-1),J={class:"text-right mr-80"},H=Object(r["i"])("p",null,"Your daily bees fact:",-1),F={class:"font-extralight italic"};Object(r["p"])();var L=i((function(e,t,n,o,l,s){var L=Object(r["w"])("tab"),Y=Object(r["w"])("vue-echarts"),z=Object(r["w"])("datepicker"),Q=Object(r["w"])("tabs");return Object(r["o"])(),Object(r["f"])("div",a,[c,u,p,Object(r["i"])("div",null,[Object(r["i"])(Q,{modelValue:o.active,"onUpdate:modelValue":t[17]||(t[17]=function(e){return o.active=e})},{default:i((function(){return[Object(r["i"])(L,{title:"Your account"},{default:i((function(){return[s.loggedIn?(Object(r["o"])(),Object(r["f"])("div",d,[Object(r["i"])("p",null,"Welcome, "+Object(r["y"])(l.user.email),1),Object(r["i"])("button",{onClick:t[1]||(t[1]=function(){return s.logout&&s.logout.apply(s,arguments)}),class:"mt-2 p-1 bg-red-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Logout ")])):(Object(r["o"])(),Object(r["f"])("div",b,[h,Object(r["i"])("div",f,[Object(r["i"])("div",null,[Object(r["E"])(Object(r["i"])("input",{"onUpdate:modelValue":t[2]||(t[2]=function(e){return l.user.email=e}),type:"text",placeholder:"email address",disabled:l.user.token_asked},null,8,["disabled"]),[[r["A"],l.user.email]]),l.user.token_asked?(Object(r["o"])(),Object(r["f"])("button",g," Authenticate ")):(Object(r["o"])(),Object(r["f"])("button",{key:0,onClick:t[3]||(t[3]=function(){return s.askToken&&s.askToken.apply(s,arguments)}),class:"ml-2 p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Authenticate "))]),l.user.token_asked?(Object(r["o"])(),Object(r["f"])("div",y,[Object(r["E"])(Object(r["i"])("input",{"onUpdate:modelValue":t[4]||(t[4]=function(e){return l.user.token=e}),type:"text",placeholder:"paste the token here"},null,512),[[r["A"],l.user.token]]),l.user.token.length>3&&!l.user.token_sent?(Object(r["o"])(),Object(r["f"])("button",{key:0,onClick:t[5]||(t[5]=function(){return s.confirmToken&&s.confirmToken.apply(s,arguments)}),class:"p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Confirm token ")):Object(r["g"])("",!0),l.user.token_sent?(Object(r["o"])(),Object(r["f"])("button",m," Confirm token ")):Object(r["g"])("",!0)])):Object(r["g"])("",!0)])]))]})),_:1}),Object(r["i"])(L,{title:"Take the poll"},{default:i((function(){return[l.displayed.poll_startDate?(Object(r["o"])(),Object(r["f"])("div",O,[Object(r["i"])("div",_,[Object(r["i"])("div",null,[Object(r["i"])("textarea",{id:"takingQuestion",rows:"2",cols:"30",value:l.displayed.poll_question,disabled:""},null,8,["value"])]),l.chart.scores.length>0&&l.displayed.poll_visible?(Object(r["o"])(),Object(r["f"])(Y,{key:0,option:l.chart.options,style:s.chartStyle,ref:"chart"},null,8,["option","style"])):Object(r["g"])("",!0),Object(r["i"])("div",null,[Object(r["i"])("textarea",{id:"takingDescription",rows:"3",cols:"45",value:l.displayed.poll_description,disabled:""},null,8,["value"])]),(Object(r["o"])(!0),Object(r["f"])(r["a"],null,Object(r["u"])(l.displayed.poll_results,(function(e,t){return Object(r["o"])(),Object(r["f"])("div",{key:t},[Object(r["i"])("div",null,[Object(r["i"])("label",null,Object(r["y"])(e.text),1),Object(r["i"])("input",{class:"ml-4",type:"checkbox",onChange:function(e){return s.toggleResults(t)},checked:e.value},null,40,["onChange","checked"])])])})),128)),Object(r["i"])("div",null,[j,Object(r["i"])("input",{class:"mx-3 mr-5",type:"checkbox",id:"takingMultiple",checked:l.displayed.poll_multiple,disabled:""},null,8,["checked"]),k,Object(r["i"])("input",{class:"mx-3 mr-5",type:"checkbox",id:"takingVisible",checked:l.displayed.poll_visible,disabled:""},null,8,["checked"])]),Object(r["i"])("div",v,[w,Object(r["i"])("input",{type:"text",id:"takingStartDate",value:l.displayed.poll_startDate,disabled:""},null,8,["value"]),l.displayed.poll_endDate?(Object(r["o"])(),Object(r["f"])("div",x,[P,Object(r["i"])("input",{type:"text",id:"takingEndDate",value:l.displayed.poll_endDate,disabled:""},null,8,["value"])])):Object(r["g"])("",!0)])]),Object(r["i"])("button",{onClick:t[6]||(t[6]=function(){return s.takePoll&&s.takePoll.apply(s,arguments)}),class:"my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Take the poll ")])):(Object(r["o"])(),Object(r["f"])("div",D,[S]))]})),_:1}),Object(r["i"])(L,{title:"Create a poll"},{default:i((function(){return[Object(r["i"])("div",q,[Object(r["i"])("div",null,[Object(r["E"])(Object(r["i"])("textarea",{id:"creatingQuestion",rows:"2",cols:"30","onUpdate:modelValue":t[7]||(t[7]=function(e){return l.creatingPoll.question=e})},null,512),[[r["A"],l.creatingPoll.question]])]),Object(r["i"])("div",null,[Object(r["E"])(Object(r["i"])("textarea",{id:"creatingDescription",rows:"3",cols:"45","onUpdate:modelValue":t[8]||(t[8]=function(e){return l.creatingPoll.description=e})},null,512),[[r["A"],l.creatingPoll.description]])]),Object(r["i"])("div",null,[(Object(r["o"])(!0),Object(r["f"])(r["a"],null,Object(r["u"])(l.creatingPoll.answers,(function(e,t){return Object(r["o"])(),Object(r["f"])("div",{class:"my-4",key:"answer"+t},[Object(r["i"])("label",{class:"mr-4",for:"creatingAnswer"+t},"Answer #"+Object(r["y"])(t+1),9,["for"]),Object(r["i"])("input",{type:"text",onInput:function(e){return l.creatingPoll.answers[t]=e.target.value}},null,40,["onInput"])])})),128))]),Object(r["i"])("div",null,[Object(r["i"])("button",{onClick:t[9]||(t[9]=function(){return s.addAnswer&&s.addAnswer.apply(s,arguments)}),class:"p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," more answers... "),l.creatingPoll.answers.length>2?(Object(r["o"])(),Object(r["f"])("button",{key:0,onClick:t[10]||(t[10]=function(){return s.removeAnswer&&s.removeAnswer.apply(s,arguments)}),class:"ml-2 p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," less ")):Object(r["g"])("",!0)]),Object(r["i"])("div",A,[C,Object(r["E"])(Object(r["i"])("input",{class:"mx-3 mr-5",type:"checkbox",id:"creatingMultiple","onUpdate:modelValue":t[11]||(t[11]=function(e){return l.creatingPoll.multiple=e})},null,512),[[r["z"],l.creatingPoll.multiple]]),$,Object(r["E"])(Object(r["i"])("input",{class:"mx-3 mr-5",type:"checkbox",id:"creatingVisible","onUpdate:modelValue":t[12]||(t[12]=function(e){return l.creatingPoll.visible=e})},null,512),[[r["z"],l.creatingPoll.visible]])]),Object(r["i"])("div",T,[R,Object(r["i"])(z,{id:"creatingStartDate",modelValue:l.creatingPoll.startDate,"onUpdate:modelValue":t[13]||(t[13]=function(e){return l.creatingPoll.startDate=e})},null,8,["modelValue"]),V,M,Object(r["i"])(z,{id:"creatingEndDate",modelValue:l.creatingPoll.endDate,"onUpdate:modelValue":t[14]||(t[14]=function(e){return l.creatingPoll.endDate=e})},null,8,["modelValue"])])]),Object(r["i"])("button",{onClick:t[15]||(t[15]=function(){return s.createPoll&&s.createPoll.apply(s,arguments)}),class:"my-5 w-1/3 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Create the poll ")]})),_:1}),s.loggedIn?(Object(r["o"])(),Object(r["f"])(L,{key:0,title:"My Polls"},{default:i((function(){return[s.mypolls.length>0?(Object(r["o"])(),Object(r["f"])("div",I,[(Object(r["o"])(!0),Object(r["f"])(r["a"],null,Object(r["u"])(s.mypolls,(function(e,t){return Object(r["o"])(),Object(r["f"])("div",{key:t,class:"grid grid-cols-6 gap-1"},[Object(r["i"])("div",null,[Object(r["i"])("a",{href:"#",onClick:function(t){return s.switchToRestored(e.pollid)}},Object(r["y"])(e.question),9,["onClick"])]),Object(r["i"])("div",null,[Object(r["i"])("button",{class:"p-1 bg-green-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75",onClick:function(t){return s.clipBoard(e.pollid,e.secret)}}," Share link ",8,["onClick"])]),Object(r["i"])("div",null,[E,Object(r["i"])("input",{class:"ml-3",disabled:"",type:"checkbox",checked:l.user.created.some((function(t){return t.pollid===e.pollid}))},null,8,["checked"])]),Object(r["i"])("div",null,[U,Object(r["i"])("input",{class:"ml-3",disabled:"",type:"checkbox",checked:l.user.taken.some((function(t){return t.pollid===e.pollid}))},null,8,["checked"])]),Object(r["i"])("div",null,Object(r["y"])(e.startDate),1),Object(r["i"])("div",null,Object(r["y"])(null!==e.endDate||"No end date"),1)])})),128))])):Object(r["g"])("",!0),Object(r["i"])("div",N,[B,Object(r["i"])("button",{onClick:t[16]||(t[16]=function(){return s.restoreHistory&&s.restoreHistory.apply(s,arguments)}),class:"mt-4 p-1 bg-yellow-500 text-white font-semibold rounded-lg shadow-md hover:bg-green-700 focus:ring-opacity-75"}," Retrieve entire history ")])]})),_:1})):Object(r["g"])("",!0)]})),_:1},8,["modelValue"]),Object(r["i"])("div",J,[H,Object(r["i"])("p",F,Object(r["y"])(l.todayFact)+".",1)])])])})),Y=n("3835"),z=n("2909"),Q=(n("caad"),n("2532"),n("1276"),n("ac1f"),n("4de4"),n("b64b"),n("d3b7"),n("d81d"),n("25f0"),n("99af"),n("13d5"),n("159b"),n("7db0"),n("4fad"),n("24e7")),G=n("6f08"),W=n("a5bb"),K={class:"mx-80 object-center"},X={class:"flex flex-row"},Z=Object(r["i"])("div",{class:"bg-gray-300 -m-1 h-1"},null,-1),ee={class:"mt-6"};function te(e,t,n,o,l,s){return Object(r["o"])(),Object(r["f"])("div",K,[Object(r["i"])("ul",X,[(Object(r["o"])(!0),Object(r["f"])(r["a"],null,Object(r["u"])(o.tabs,(function(e,t){return Object(r["o"])(),Object(r["f"])("li",{key:t,class:[o.active===t?"border-b-4 border-yellow-500 text-gray-800":"border-b-2 border-white text-gray-500","flex items-center px-6 py-3 rounded-tl-md rounded-tr-md overflow-hidden cursor-pointer hover:text-gray-800"],onClick:function(e){return o.selectTab(t)}},Object(r["y"])(e.props.title),11,["onClick"])})),128))]),Z,Object(r["i"])("div",ee,[Object(r["v"])(e.$slots,"default")])])}n("a9e3");var ne={name:"Tabs",props:{modelValue:{type:[String,Number]}},emits:["update:modelValue"],setup:function(e,t){var n=t.emit,o=Object(r["d"])((function(){return e.modelValue})),l=Object(r["s"])([]);function s(e){n("update:modelValue",e)}return Object(r["q"])("tabsState",{active:o,tabs:l}),{tabs:l,active:o,selectTab:s}}};ne.render=te;var re=ne,oe={key:0};function le(e,t,n,o,l,s){return o.isActive?(Object(r["o"])(),Object(r["f"])("div",oe,[Object(r["v"])(e.$slots,"default")])):Object(r["g"])("",!0)}n("c740");var se={name:"Tab",props:{title:String},setup:function(){var e=Object(r["k"])(),t=Object(r["m"])("tabsState"),n=t.tabs,o=t.active,l=Object(r["d"])((function(){return n.value.findIndex((function(t){return t.uid===e.uid}))})),s=Object(r["d"])((function(){return l.value===o.value}));return Object(r["C"])((function(){-1===l.value&&n.value.push(e)})),{isActive:s}}};se.render=le;var ie=se,ae=null,ce=null,ue={noLocalStorage:"`localStorage API` not supported, this application will not work properly.",noCookieSet:"Sorry but you appear to have cleared your cookies",loaded:"App loaded successfully.",endpoint:"The endpoint ran into an error: "},pe={status:!0,check:function(){return localStorage||(this.status=!1),this.status},set:function(e){localStorage.setItem("pollem-user",JSON.stringify(e))},get:function(){return JSON.parse(localStorage.getItem("pollem-user"))},clear:function(){localStorage.removeItem("pollem-user")}},de={server_url:{dev:"http://localhost:8009",prod:"https://pollem-now.herokuapp.com"},checkURI:function(e){if(!e.includes("polls"))return{pollid:null,secret:null};var t=e.split("#")[1].split("/")[2];return t.includes("?")?{pollid:t.split("?")[0],secret:t.split("=")[1]}:{pollid:t.split("?")[0],secret:null}},valid_keys:{post:{ask_token:{req:["ask_email"],resp:["resp_ask_token"]},close:{req:["close_hash","close_token","close_pollid"],resp:["resp_close_msg"]},confirm_token:{req:["confirm_token","confirm_fingerprint","confirm_email"],resp:["resp_confirm_msg","resp_confirm_token","resp_confirm_hash"]},create:{req:["create_hash","create_token","create_recipe","create_startDate","create_recipe"],resp:["resp_create_msg","resp_create_pollid","resp_create_pollsecret"]},myhistory:{req:["myhistory_hash","myhistory_token"],resp:["resp_myhistory_polls","resp_myhistory_taken","resp_myhistory_created","resp_myhistory_msg"]},take:{req:["take_fingerprint","take_hash","take_token","take_results"],resp:["resp_take_msg"]}},get:{polls:{resp:["resp_get_poll_msg","resp_get_poll","resp_get_poll_scores"]},warmup:{resp:["resp_warmup_msg"]}}},tryPayload:function(e,t){var n=t.filter((function(t){return!Object.keys(e).includes(t)}));if(n.length>0)throw new Error("Missing key(s) from payload: "+JSON.stringify(n))},tryRoute:function(e,t){if(!Object.keys(this.valid_keys[e]).includes(t))throw new Error("Invalid route: "+e+" and route "+t)},makeReq:function(e,t){var n=this,r=arguments.length>2&&void 0!==arguments[2]?arguments[2]:null;this.tryRoute(e,t);var o=this.server_url[this.AppMode],l=o+"/"+t;if("polls"===t&&(l=l+"/"+ae+"?secret="+ce),"get"===e)return fetch(l).then((function(e){return e.json()})).then((function(r){return n.tryPayload(r,n.valid_keys[e][t].resp),r}));if(null===r)throw new Error("Empty payload found in "+l+"request.");this.tryPayload(r,this.valid_keys[e][t].req);var s={headers:{"Content-Type":"application/json",Accept:"application/json"},method:"POST",body:JSON.stringify(r)};return fetch(l,s).then((function(e){return e.json()})).then((function(r){return n.tryPayload(r,n.valid_keys[e][t].resp),r}))}},be={name:"requests",components:{Tabs:re,Tab:ie,VueEcharts:G["a"],Datepicker:Q["a"]},data:function(){return{AppMode:"dev",Host:"https://hardcore-hopper-66afd6.netlify.app",creatingPoll:{startDate:null,endDate:null,question:"question...",description:"description...",multiple:!1,visible:!1,private:!1,answers:["Answer#1","Answer#2"]},displayed:{},chart:{scores:[],options:{}},user:{token:"",hash:"",email:"",fingerprint:"",created:[],taken:[],token_asked:!1,token_sent:!1},bees_facts:["Bees have 5 eyes","Bees are insects, so they have 6 legs","Male bees in the hive are called drones","Bees fly about 20 mph","Female bees in the hive (except the queen) are called worker bees","Number of eggs laid by queen: 2,000 per day is the high","Losing its stinger will cause a bee to die","Bees have been here about 30 million years!","Bees carry pollen on their hind legs in a pollen basket or corbicula","An average beehive can hold around 50,000 bees","Foragers must collect nectar from about 2 million flowers to make 1 pound of honey","The average forager makes about 1/12 th of a teaspoon of honey in her lifetime","Average per capita honey consumption in the US is 1.3 pounds","Bees have 2 pairs of wings","The principal form of communication among honey bees is through chemicals called pheromones","Bees are important because they pollinate approximately 130 agricultural crops in the US including fruit, fiber, nut, and vegetable crops. Bee pollination adds approximately 14 billion dollars annually to improved crop yield and quality."],todayFact:""}},setup:function(){var e,t=de.checkURI(window.location.href),n=t.pollid,o=t.secret;return null!==n?(ae=parseInt(n),ce=o,e=Object(r["s"])(1)):e=Object(r["s"])(0),{active:e}},mounted:function(){var e=this;W["a"].load().then((function(e){return e.get()})).then((function(t){pe.check()?(e.user=null===pe.get()?e.user:pe.get(),e.user.fingerprint=t.visitorId):e.$toast.error(ue.noLocalStorage);var n=Math.floor(Math.random()*e.bees_facts.length);if(e.todayFact=e.bees_facts[n],e.creatingPoll.startDate=new Date,null!==ae)return de.makeReq("get","polls").catch((function(t){return e.$toast.error(t)})).then((function(t){e.tryPayload(t,e.valid_keys.get.polls.resp);var n=t.resp_get_poll;e.displayed=Object.assign(e.displayed,n),n.poll_endDate||(e.displayed.poll_endDate=null),e.displayed.answers&&(e.displayed.answers=n.poll_answers.map((function(e){return{text:e,value:!1}}))),t.resp_get_poll_scores&&(e.chart.scores=t.resp_get_poll_scores.map((function(e){return parseInt(e)})),e.setChartOptions(e.displayed.poll_answers,e.chart.scores)),e.$toast.success(ue.loaded+" Here is your poll. ("+t.resp_get_poll_msg+")")}));de.makeReq("get","warmup").catch((function(t){return e.$toast.error(t)})).then((function(t){return e.$toast.info(t.resp_warmup_msg)}))}))},computed:{loggedIn:function(){return[this.user.token,this.user.hash,this.user.email,this.user.fingerprint].every((function(e){return""!==e}))},chartStyle:function(){var e=this.displayed.poll_answers.length;return 0===e?null:"width: 900px; height: "+(75*e).toString()+"px"},mypolls:function(){var e=[].concat(Object(z["a"])(this.user.created),Object(z["a"])(this.user.taken));return e.reduce((function(e,t){if(e[0].includes(t.pollid))return e;var n=[].concat(Object(z["a"])(e[0]),[t.pollid]),r=[].concat(Object(z["a"])(e[1]),[t]);return[n,r]}),[[],[]]).pop()}},methods:{switchToRestored:function(e){var t=this;return fetch(de.server_url[this.AppMode]+"/polls/"+e).then((function(e){return e.json()})).then((function(n){t.tryPayload(n,t.valid_keys.get.polls.resp);var r=n.resp_get_poll;t.displayed=Object.assign(t.displayed,r),r.poll_endDate||(t.displayed.poll_endDate=null),t.displayed.poll_answers&&(t.displayed.poll_results=r.poll_answers.map((function(e){return{text:e,value:!1}}))),null!==n.resp_get_poll_scores&&(t.chart.scores=n.resp_get_poll_scores.map((function(e){return parseInt(e)})),t.setChartOptions(t.displayed.poll_answers,t.chart.scores)),ae=e,t.active=1}))},setChartOptions:function(e,t){this.chart.options={yAxis:{type:"category",data:e},xAxis:{type:"value"},series:[{data:t,type:"bar"}]}},clipBoard:function(e,t){var n=this,r=this.Host+"/#/polls/"+e+"?secret="+t;navigator.clipboard.writeText(r).then((function(){return n.$toast.info("Copied to clipboard.")})).catch((function(){return n.$toast.warning("Unable to copy, please open the link and copy from your URL bar")}))},toggleResults:function(e){this.displayed.poll_results[e].value=!this.displayed.poll_results[e].value,this.displayed.poll_multiple||this.displayed.poll_results.forEach((function(t,n){return t.value=n===e&&t.value}))},addAnswer:function(){var e=this.creatingPoll.answers.length+1;this.creatingPoll.answers.push("Answer#"+e.toString())},removeAnswer:function(){this.creatingPoll.answers.pop()},makeReq:function(e,t,n){var r=this,o=de.server_url[this.AppMode],l=de.tryRoute(e,t);if(null!==l){var s=o+l;if(de.routes.get.some((function(e){return e===l})))return fetch(s).catch((function(e){return r.$toast.error(e)})).then((function(e){return e.json()}));var i=de.post[t].filter((function(e){return!Object.keys(n).includes(e)}));if(!(i.length>0)){var a={headers:{"Content-Type":"application/json",Accept:"application/json"},method:"POST",body:JSON.stringify(n)};return fetch(s,a).catch((function(e){return r.$toast.error(e)})).then((function(e){return e.json()}))}this.$toast.error("Missing key(s) from payload: "+JSON.stringify(i))}else this.$toast.error("Bad endpoint! Request aborted.")},askToken:function(){var e=this;this.user.token_asked=!0;var t={ask_email:this.user.email};return de.makeReq("post","ask_token",t).catch((function(t){return e.$toast.error(t)})).then((function(t){e.$toast.success(t.resp_ask_token,{duration:1e4})}))},confirmToken:function(){var e=this;this.user.token_sent=!0;var t={confirm_token:this.user.token,confirm_fingerprint:this.user.fingerprint,confirm_email:this.user.email};return de.makeReq("post","confirm_token",t).catch((function(t){return e.$toast.error(t)})).then((function(t){t.resp_confirm_token&&t.resp_confirm_hash?(e.user.token=t.resp_confirm_token,e.user.hash=t.resp_confirm_hash,e.$toast.success(t.resp_confirm_msg)):e.$toast.warning(t.resp_confirm_msg)}))},createPoll:function(){var e=this,t={poll_startDate:this.creatingPoll.startDate,poll_question:this.creatingPoll.question,poll_description:this.creatingPoll.description,poll_multiple:this.creatingPoll.multiple,poll_visible:this.creatingPoll.visible,poll_answers:this.creatingPoll.answers},n={create_hash:this.user.hash,create_token:this.user.token,create_startDate:this.creatingPoll.startDate};null!==this.creatingPoll.endDate&&(t.poll_endDate=this.creatingPoll.endDate,n.create_endDate=this.creatingPoll.endDate);var r=["poll_startDate","poll_question","poll_description","poll_visible","poll_multiple","poll_answers"].filter((function(e){return!Object.keys(t).includes(e)}));if(!(r.length>0))return n.create_recipe=JSON.stringify(t),de.makeReq("post","create",n).catch((function(t){return e.$toast.error(t)})).then((function(t){e.$toast.success(t.resp_create_msg);var n={question:e.creatingPoll.question,startDate:e.creatingPoll.startDate,secret:t.resp_create_pollsecret,pollid:t.resp_create_pollid};null!==e.creatingPoll.endDate&&(n.endDate=e.creatingPoll.endDate),e.user.created.push(n)}));this.$toast.error("Missing key from poll: "+JSON.stringigify(r))},closePoll:function(){var e=this,t={close_hash:this.user.hash,close_token:this.user.token,close_pollid:ae.toString()};return de.makeReq("post","close",t).catch((function(t){return e.$toast.error(t)})).then((function(t){return e.$toast.success(t.resp_close_msg)}))},takePoll:function(){var e=this,t={take_fingerprint:this.user.fingerprint,take_hash:this.user.hash,take_token:this.user.token,take_results:this.displayed.poll_answers.map((function(t){return e.displayed.poll_results.find((function(e){return e.text===t})).value?1:0})),take_pollid:ae.toString()};return de.makeReq("post","take",t).catch((function(t){return e.$toast.error(t)})).then((function(t){return e.$toast.success(t.resp_take_msg)}))},restoreHistory:function(){var e=this,t={myhistory_hash:this.user.hash,myhistory_token:this.user.token};return this.user.created=[],this.user.taken=[],de.makeReq("post","myhistory",t).catch((function(t){return e.$toast.error(t)})).then((function(t){if(null!==t.resp_myhistory){e.$toast.success(t.resp_myhistory_msg);for(var n=t.resp_myhistory_polls,r=t.resp_myhistory_created,o=t.resp_myhistory_taken,l=0,s=Object.entries(n);l<s.length;l++){var i=Object(Y["a"])(s[l],2),a=i[0],c=i[1],u=JSON.parse(c[2][1]),p=new Date(u.poll_startDate),d=c[1][1],b={question:u.poll_question,startDate:p,secret:d,pollid:a};null!==u.poll_endDate&&(b.endDate=new Date(u.poll_endDate)),r.includes(a)&&e.user.created.push(b),o.includes(a)&&e.user.taken.push(b)}}else e.$toast.error(t.resp_myhistory_msg)}))},logout:function(){this.user={token:"",hash:"",email:"",fingerprint:"",created:[],taken:[],token_asked:!1,token_sent:!1},pe.clear()}},watch:{user:{deep:!0,handler:function(e){[this.user.token,this.user.hash,this.user.email,this.user.fingerprint,this.user.created,this.user.taken].every((function(e){return""!==e}))&&pe.set(e)}}}};n("bab2");be.render=L,be.__scopeId="data-v-84e92d88";var he=be,fe={name:"App",components:{Requests:he}};n("3503");fe.render=o;var ge=fe,ye=n("af56");n("a766");Object(r["e"])(ge).use(ye["a"],{position:"top-left"}).mount("#app")},"610a":function(e,t,n){e.exports=n.p+"img/bees_thumb.93bd0569.png"},"80b5":function(e,t,n){},a766:function(e,t,n){},bab2:function(e,t,n){"use strict";n("47f1")}});
//# sourceMappingURL=app.2d6de5aa.js.map