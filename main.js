window.onload = function() {
  var $ = document.getElementById.bind(document);
  var api = scalisp.Main();
  var boot = null;

  var $input = $('input');
  var $output = $('output');

  function macroExpand() {
    $output.innerHTML = '';
    var ctx = api.createContext(boot);
    var program = api.parse($input.value);
    for (var i=0; i<program.length; ++i) {
      var s = api.macroExpand(ctx, true, program[i]);
      api.printValue(s);
    }
  }

  function compile() {
    $output.innerHTML = '';
    var ctx = api.createContext(boot);
    var program = api.parse($input.value);
    for (var i=0; i<program.length; ++i) {
      var s = api.macroExpand(ctx, true, program[i]);
      var c = api.compile(ctx, s);
      api.printCode(c);
    }
  }

  function run() {
    $output.innerHTML = '';
    var ctx = api.createContext(boot);
    var program = api.parse($input.value);
    api.exec(ctx, program);
  }

  function runTests() {
    $output.innerHTML = 'loading testcases...';
    load('./lispboot/test', function(text) {
      $output.innerHTML = '';
      api.runTests(text);
    });
  }

  function loadExample(filename) {
    $output.innerHTML = 'loading ' + filename + '...';
    load('./lispboot/examples/' + filename, function(text) {
      $input.value = text;
      $output.innerHTML = '';
    });
  }

  console.log = function(msg) {
    $output.innerHTML = $output.innerHTML + msg + "\n";
  };

  console.warn = console.error = function(msg) {
    $output.innerHTML = $output.innerHTML + "<span class=\"error\">" + msg + "</span>\n";
  };

  $output.innerHTML = 'loading boot code...';

  load('./lispboot/boot.lisp', function(bootCode) {
    boot = api.parse(bootCode);

    $('examples').addEventListener('change', e => loadExample(e.target.value));
    $('macroexpand').addEventListener('click', macroExpand);
    $('compile').addEventListener('click', compile);
    $('run').addEventListener('click', run);
    $('runtests').addEventListener('click', runTests);

    loadExample('hello.lisp');
  });
};

function load(filename, callback) {
  var xhr = new XMLHttpRequest();
  xhr.onreadystatechange = function() {
    if (xhr.readyState != 4) return;
    if (xhr.status == 200) {
      callback(xhr.responseText);
    } else {
      console.error('Cannot load: ' + filename);
    }
  };
  xhr.open('GET', filename, true);
  xhr.send();
}
