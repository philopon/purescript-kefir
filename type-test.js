var K = require('kefir')

function streamType(s){
  if(s._current === undefined){
    return "stream";
  } else {
    return "prop";
  }
}

var str = K.emitter();
var prp = K.constant();

function logTest(name, fn) {
  console.log(name + '\t' + streamType(fn(str)) + '\t' + streamType(fn(prp)));
}

function logTest2(name, fn) {
  console.log(name + '\t' + streamType(fn(str, str)) + '\t' + streamType(fn(str,prp)) + '\t' + streamType(fn(prp,str)) + '\t' + streamType(fn(prp,prp)));
}

function ident(x){
  return x;
}

function add(a,b){
  return a + b;
}

function constant(a){
  return function(){
    return a;
  }
}

console.log('name\tstream\tprop')
logTest("map", function(s){return s.map(ident)});
logTest("filter", function(s){return s.filter()});
logTest("take", function(s){return s.take(10)});
logTest("takeWhile", function(s){return s.takeWhile()});
logTest("skip", function(s){return s.skip(10)});
logTest("skipWhile", function(s){return s.skipWhile()});
logTest("skipDuplicates", function(s){return s.skipDuplicates()});
logTest("diff", function(s){return s.diff()});
logTest("scan", function(s){return s.scan(add)});
logTest("reduce", function(s){return s.reduce(add)});
logTest("mapEnd", function(s){return s.mapEnd(constant(10))});
logTest("skipEnd", function(s){return s.skipEnd(constant(10))});
logTest("slidingWindow", function(s){return s.slidingWindow(10)});
logTest("bufferWhile", function(s){return s.bufferWhile()});
logTest("delay", function(s){return s.delay(10)});
logTest("throttle", function(s){return s.throttle(10)});
logTest("debounce", function(s){return s.debounce(10)});
logTest("flatten", function(s){return s.flatten()});
logTest("withHandler", function(s){return s.flatten(undefined)});

logTest("flatMap", function(s){return s.flatMap(s)});
logTest("flatMapLatest", function(s){return s.flatMapLatest(s)});
logTest("flatMapFirst", function(s){return s.flatMapFirst(s)});
logTest("flatMapConcat", function(s){return s.flatMapConcat(s)});
logTest("flatMapConcurLimit", function(s){return s.flatMapConcurLimit(s)});

console.log("\nname\tstr-str\tstr-prp\tprp-str\tprp-prp");
logTest2("combine", function(a,b){return K.combine([a,b])});
logTest2("and", function(a,b){return K.and([a,b])});
logTest2("or", function(a,b){return K.or([a,b])});
logTest2("sampledBy", function(a,b){return K.sampledBy([a,a], [b,b])});
logTest2("zip", function(a,b){return K.zip([a,b])});
logTest2("merge", function(a,b){return K.merge([a,b])});
logTest2("concat", function(a,b){return K.concat([a,b])});
logTest2('filterBy', function(a,b){return a.filterBy(b);});
logTest2('takeWhileBy', function(a,b){return a.takeWhileBy(b);});
logTest2('skipWhileBy', function(a,b){return a.skipWhileBy(b);});
logTest2('skipUntilBy', function(a,b){return a.skipUntilBy(b);});
logTest2('takeUntilBy', function(a,b){return a.takeUntilBy(b);});
logTest2('bufferBy', function(a,b){return a.bufferBy(b);});
logTest2('bufferWhileBy', function(a,b){return a.bufferWhileBy(b);});
logTest2('awaiting', function(a,b){return a.awaiting(b);});
