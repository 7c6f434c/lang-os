
let contentScriptStackInterpreter = async function(code){
        let contentScriptStackInterpreterStep = async function(stack,dict,c){
                if(typeof(c)=="object"){
                        switch(c.op){
                                case "store":
                                        var v = stack.pop();
                                        dict[c.key] = v;
                                        break;
                                case "storeLoad":
                                        var v = stack.pop();
                                        stack.push(v);
                                        dict[c.key] = v;
                                        break;
                                case "load":
                                        stack.push(dict[c.key]);
                                        break;
                                case "storeThis":
                                        var v = stack.pop();
                                        this[c.key] = v;
                                        break;
                                case "storeLoadThis":
                                        var v = stack.pop();
                                        stack.push(v);
                                        this[c.key] = v;
                                        break;
                                case "loadThis":
                                        stack.push(this[c.key]);
                                        break;
                                case "gather":
                                        var n = c.n;
                                        var l = [];
                                        for(i=0; i<n; i+=1){
                                                l[n-i-1]=stack.pop();
                                        }
                                        stack.push(l);
                                        break;
                                case "setAttr":
                                        var v = stack.pop();
                                        var o = stack.pop();
                                        o[c.key]=v;
                                        stack.push(o);
                                        break;
                                case "getAttr":
                                        var o = stack.pop();
                                        stack.push(o[c.key]);
                                        break;
                                case "getAttrPath":
                                        var o = stack.pop();
                                        var v = o;
                                        for (x of c.key){
                                                v = v[x];
                                        }
                                        stack.push(v);
                                        break;
                                case "dup":
                                        var o = stack.pop();
                                        stack.push(o);
                                        stack.push(o);
                                        break;
                                case "call":
                                        var a = stack.pop();
                                        var f = stack.pop();
                                        var o = stack.pop();
                                        stack.push(await f.apply(o,a));
                                        break;
                                case "method":
                                        var a = stack.pop();
                                        var o = stack.pop();
                                        var f = o[c.key];
                                        stack.push(await f.apply(o,a));
                                        break;
                                case "..":
                                case "api":
                                        var name = c.payload;
                                        var o = this;
                                        var f = this;
                                        var call = true;
                                        for(var x of name.split(/[.]/)){
                                                o = f;
                                                f = f[x];
                                                if (x == ""){
                                                        call = false;
                                                }
                                        }
                                        if(call){
                                                var a = stack.pop();
                                                stack.push(await f.apply(o,a));
                                        }else{
                                                stack.push(o);
                                        }
                                        break;
                                case ".":
                                case "subAPI":
                                        var name = c.payload;

                                        var call = name[name.length-1]!=".";
                                        var a = null;
                                        if(call){
                                                a=stack.pop();
                                        }

                                        var o = stack.pop();
                                        var f = o;
                                        for(var x of name.split(/[.]/)){
                                                o = f;
                                                f = f[x];
                                                if (x == ""){
                                                        call = false;
                                                }
                                        }

                                        if(call){
                                                stack.push(await f.apply(o,a));
                                        }else{
                                                stack.push(o);
                                        }
                                        break;
                                case "pop":
                                        stack.pop();
                                        break;
                                case "":
                                case "data":
                                        stack.push(c.data);
                                        break;
                                case "document":
                                        stack.push(document);
                                        break;
                                case "window":
                                        stack.push(window);
                                        break;
                                case "browser":
                                        stack.push(browser);
                                        break;
                                case "chrome":
                                        stack.push(chrome);
                                        break;
                                case "this":
                                        stack.push(this);
                                        break;
                                case "eval":
                                        var x=stack.pop();
                                        await contentScriptStackInterpreterStep(stack,dict,x);
                                        break;
                                case "code":
                                        var d = {};
                                        for(x of c.payload){
                                                if (typeof(x)=="object"){
                                                        if(Array.isArray(x)){
                                                                d.op=x[0];
                                                                d.key=x[1];
                                                                d.payload=x[1];
                                                                d.data=x[1];
                                                                d.n=x[1];
                                                                await contentScriptStackInterpreterStep(
                                                                        stack,dict,d);
                                                                d={};
                                                        }else{
                                                                d=x;
                                                        }
                                                }else if(typeof(x)=="string"){
                                                        d.op=x;
                                                        await contentScriptStackInterpreterStep(stack,dict,d);
                                                        d={};
                                                }else{
                                                        stack.push(x);
                                                        d={};
                                                }
                                        };
                                        break;
                                case "shadowMap":
                                        var o = stack.pop();
                                        var res = {};
                                        if(Array.isArray(o)){
                                                res = [];
                                        }
                                        for (k in o){
                                                var substack = [k, o[k]];
                                                await contentScriptStackInterpreterStep(substack,dict,c.payload);
                                                res[k]=substack.pop();
                                        }
                                        stack.push(res);
                                        break;
                                case "debug":
                                        console.log(stack.length);
                                        console.log(stack);
                                        console.log(dict);
                                        break;
                        }
                }else{
                        stack.push(c);
                }
                return null;
        }

        var stack = [];
        var dict = {};
        for (c of code){
                await contentScriptStackInterpreterStep(stack,dict,c);
        }
        var result = stack.pop();
        try{
                return {
                        "type": typeof(result),
                        "string": ((result || {}).toString || function(){
                                return ""
                        }).apply(result,[]),
                        "JSON": JSON.stringify(result),
                        "value": JSON.parse(JSON.stringify(result) || "{}"),
                }
        }catch{
                rd = {"$":"un-JSON-friendly object"}
                return {
                        "type": typeof(result),
                        "string": ((result || {}).toString || function(){
                                return ""
                        }).apply(result,[]),
                        "JSON": JSON.stringify(rd),
                        "value": JSON.parse(JSON.stringify(rd) || "{}"),
                }
        }
}

async function handleNativeMessage(response) {
  try{
  console.log(`Received: ${JSON.stringify(response)}`);
  if (Array.isArray(response)){
          var data = response;
          response = {
                  "op":data[0],
                  "payload":data[1],
          };
  }
  if(typeof(response)=="object"){
          var result = {};
          var tabq = await browser.tabs.query({
                  "active": true, 
                  "currentWindow": true
          });
          if(tabq.length != 1){
                  result = {"error":"Cannot select tab","extraData":tabq};
          }else{
                  var tab = tabq[0];
                  switch(response.op){
                          case "dumpHTML":
                                  result = await browser.scripting.executeScript({
                                          "func": function () {
                                                  return document.documentElement.outerHTML;
                                          },
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "dumpURL":
                                  result = await browser.scripting.executeScript({
                                          "func": function () {
                                                  return document.location.href;
                                          },
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "injectCode":
                                  result = await browser.scripting.executeScript({
                                          "func": async function(code){return eval(code);},
                                          "args": [response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "selectOne":
                                  result = await browser.scripting.executeScript({
                                          "func": async function(code){return document.querySelector(code).outerHTML;},
                                          "args": [response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "selectAll":
                                  result = await browser.scripting.executeScript({
                                          "func": async function(code){return document.querySelectorAll(code).map(x=>x.outerHTML);},
                                          "args": [response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "addHTML":
                                  result = await browser.scripting.executeScript({
                                          "func": async function(selector,pos,html){
                                                  var el = document.querySelector(selector);
                                                  if(el){
                                                          el.insertAdjacentHTML(pos,html)
                                                  }
                                          },
                                          "args": [response.selector || "body", response.pos || "afterend", response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "go":
                                  result = await browser.scripting.executeScript({
                                          "func": async function(url){
                                                  await document.location.replace(url);
                                                  return url;
                                          },
                                          "args": [response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "interpretStack":
                                  result = await browser.scripting.executeScript({
                                          "func": contentScriptStackInterpreter,
                                          "args": [response.payload],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "interpretStackCode":
                                  result = await browser.scripting.executeScript({
                                          "func": contentScriptStackInterpreter,
                                          "args": [[{"op":"code","payload":response.payload}]],
                                          "target": { "tabId": tab.id }
                                  });
                                  break;
                          case "interpretStackCodeBrowser":
                                  result = await contentScriptStackInterpreter(
                                          [{"op":"code","payload":response.payload}]
                                  );
                                  break;
                          case "formfill":
                                  result=await browser.scripting.executeScript({
                                          "func": async function(payload) {
                                                  for(var i in payload){
                                                          var entry = payload[i];
                                                          var selector = entry[0];
                                                          var value = entry[1];
                                                          var click = entry[2];
                                                          var el = null;
                                                          while (! el){
                                                                  el = document.querySelector(selector);
                                                                  if(!el){
                                                                          await new Promise(r => 
                                                                                  setTimeout(r, 100));
                                                                  }
                                                          }
                                                          if(click){
                                                                  el.click()
                                                          }else{
                                                                  el.focus();
                                                                  var dispatchBoth = function (ks,name){
                                                                          var ev = new this[ks](name,{
                                                                                          bubbles:true,
                                                                                          cancellable:false,
                                                                                          target:el
                                                                                  });
                                                                  //        //var evx = new this.
                                                                  //        //        wrappedJSObject[ks](name,{
                                                                  //        //                bubbles:true,
                                                                  //        //                cancellable:false,
                                                                  //        //                target:el.wrappedJSObject
                                                                  //        //        });
                                                                          var evdoc = document.createEvent(ks);
                                                                  //        //var evdocx = document.
                                                                  //        //        wrappedJSObject.
                                                                  //        //        createEvent(ks);
                                                                          evdoc.initEvent(name,true,false);
                                                                  //        //evdocx.initEvent(name,true,false);
                                                                          el.dispatchEvent(ev);
                                                                          el.dispatchEvent(evdoc);
                                                                  //        //el.wrappedJSObject.dispatchEvent(ev);
                                                                  //        //el.wrappedJSObject.dispatchEvent(evx);
                                                                  //        //el.wrappedJSObject.dispatchEvent(evdoc);
                                                                  //        //el.wrappedJSObject.dispatchEvent(evdocx);
                                                                          return null;
                                                                  }

                                                                  el.value = value;

                                                                  dispatchBoth("KeyboardEvent", "keydown");
                                                                  dispatchBoth("KeyboardEvent", "keypress");
                                                                  dispatchBoth("KeyboardEvent", "keyup");
                                                                  dispatchBoth("Event", "input");
                                                                  dispatchBoth("Event", "change");

                                                                  for(x of Object.keys(el.wrappedJSObject)){
                                                                          if(x.match(/^__reactProps/)){
                                                                                  var f =
                                                                                          el.
                                                                                          wrappedJSObject[x].
                                                                                          onChange;
                                                                                  if(f){
                                                                                          f(cloneInto({
                                                                                                  target:
                                                                                                  el.wrappedJSObject
                                                                                          }, this.wrappedJSObject, {
                                                                                                  wrapReflectors: 
                                                                                                  true
                                                                                          }));
                                                                                  }
                                                                          }
                                                                  }
                                                          }
                                                  }
                                                  return payload.length;
                                          },
                                          "args": [response.payload],
                                          "target": {"tabId": tab.id}
                                  });
                                  break;
                          case "retargetForms":
                                  result=await browser.scripting.executeScript({
                                          "func": async function() {
                                                  var fr=document.createElement("iframe");
                                                  fr.name="target_iframe";
                                                  document.body.appendChild(fr);
                                                  var fs=document.querySelectorAll("form");
                                                  for(var i=0;i<fs.length;i+=1){
                                                          fs[i].target="target_iframe";
                                                  }
                                                  return fs.length;
                                          },
                                          "args": [],
                                          "target": {"tabId": tab.id}
                                  });
                                  break;
                          case "markTextFields":
                                  result=await browser.scripting.executeScript({
                                          "func": async function() {
                                                  for(var x of document.querySelectorAll(
                                                          "form input[type=text]")){
                                                          x.value=x.name + 
                                                                  " # " + x.id + 
                                                                  " @ " + x.form.name;
                                                  }
                                                  for(var x of document.querySelectorAll(
                                                          "form input[type=password]")){
                                                          x.type="text";
                                                          x.value=x.name + 
                                                                  " # " + x.id + 
                                                                  " @#@ " + x.form.name;
                                                  }
                                          },
                                          "args": [],
                                          "target": {"tabId": tab.id}
                                  });
                                  break;
                          case "markTextFields":
                                  result=await browser.scripting.executeScript({
                                          "func": async function() {
                                                  for(var x of document.querySelectorAll(
                                                          "form input[type=text]")){
                                                          x.value=x.name + " @ " + x.form.name;
                                                  }
                                                  for(var x of document.querySelectorAll(
                                                          "form input[type=password]")){
                                                          x.type="text";
                                                          x.value=x.name + " @#@ " + x.form.name;
                                                  }
                                          },
                                          "args": [],
                                          "target": {"tabId": tab.id}
                                  });
                                  break;
                          case "killStyle":
                                  result=await browser.scripting.executeScript({
                                          "func": async function() {
                                                  var killStyle=function(x) {
                                                          if(x.removeAttribute) { 
                                                                  var badAttrs=[
                                                                          "style", "class", 
                                                                          "background", "bgcolor", "color", 
                                                                          "width", "text"
                                                                  ];
                                                                  for (y in badAttrs){
                                                                          x.removeAttribute(badAttrs[y]);
                                                                  };
                                                          };
                                                          if(x.children) { 
                                                                  for (y in x.children) { 
                                                                          killStyle(x.children[y]);
                                                                          if(y.tagName && 
                                                                                  (y.tagName.toUpperCase() == 
                                                                                          "STYLE")) {
                                                                                  x.removeChild(y);
                                                                          };
                                                                  };
                                                          };
                                                  };
                                                  var killHead = function () {var de = document.documentElement;
                                                          var i;
                                                          for (i=0; i<de.children.length;i+=1) { 
                                                                  var c = de.children[i];
                                                                  if (c && c.tagName.toUpperCase()=="HEAD") {
                                                                          de.removeChild(c);
                                                                  };
                                                          };
                                                  };
                                                  killStyle(document.documentElement);
                                                  killHead();
                                                  return "ok";
                                          },
                                          "args": [],
                                          "target": {"tabId": tab.id}
                                  });
                                  break;
                          case "closeTab":
                                  await browser.tabs.remove(tab.id);
                                  result = null;
                                  break;
                          case "reloadTab":
                                  await browser.tabs.reload(tab.id, 
                                          {"bypassCache": response.payload});
                                  result = null;
                                  break;
                          case "newTab":
                                  result = await browser.tabs.create({
                                          "url": response.payload,
                                  });
                                  break;
                  }
          }
          console.log("Replying");
          console.log(result);
          port.postMessage(result || {});
  }else{
    port.postMessage(`Pong: ${response}`);
  }
  }finally{
  }
}

let port = null;

/*
Listen for messages from the app.
*/
function connectNativeMessaging(){
  if(! port){
    port = browser.runtime.connectNative("socket_listener");
    port.onMessage.addListener(handleNativeMessage);
    port.onDisconnect.addListener(() => {
            port = null;
            connectNativeMessaging();
    });
  }
}

connectNativeMessaging();

browser.runtime.onInstalled.addListener(connectNativeMessaging);

/*
On a click on the browser action, send the app a message.
*/
/*
browser.action.onClicked.addListener(() => {
  console.log("Sending:  ping");
  connectNativeMessaging();
});
*/
