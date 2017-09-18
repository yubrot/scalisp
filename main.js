window.onload = function() {
  var $ = document.getElementById.bind(document);
  var bootProgram = null;

  var $input = $('input');
  var $output = $('output');

  var vconsole = {
    escape: function(s) {
      var tags = { '&': '&amp;', '<': '&lt;', '>': '&gt;' };
      return s.replace(/[&<>]/g, tag => tags[tag] || tag);
    },
    clear: function() {
      $output.innerHTML = '';
    },
    out: function(s) {
      $output.innerHTML = $output.innerHTML + this.escape(s);
    },
    err: function(s) {
      $output.innerHTML = $output.innerHTML + '<span class="error">' + this.escape(s) + '</span>';
    },
  };

  scalisp.stdout = bytes => vconsole.out(String.fromCharCode.apply(String, bytes));
  scalisp.stderr = bytes => vconsole.err(String.fromCharCode.apply(String, bytes));

  function withContext(f) {
    try {
      var ctx = scalisp.createContext();
      scalisp.initContext(ctx, bootProgram);
      f(ctx);
    } catch (e) {
      vconsole.err(e + '\n');
    }
  }

  function macroExpand() {
    vconsole.clear();
    withContext(ctx => {
      var program = scalisp.parse($input.value);
      for (var i=0; i<program.length; ++i) {
        var s = scalisp.macroExpand(ctx, true, program[i]);
        vconsole.out(scalisp.inspectValue(s) + "\n");
      }
    });
  }

  function compile() {
    vconsole.clear();
    withContext(ctx => {
      var program = scalisp.parse($input.value);
      for (var i=0; i<program.length; ++i) {
        if (i != 0) vconsole.out("------\n");
        var s = scalisp.macroExpand(ctx, true, program[i]);
        var c = scalisp.compile(ctx, s);
        vconsole.out(scalisp.printCode(c));
      }
    });
  }

  function run() {
    vconsole.clear();
    withContext(ctx => {
      var program = scalisp.parse($input.value);
      scalisp.exec(ctx, program);
    });
  }

  function runTests() {
    vconsole.clear();
    vconsole.out('loading testcases...\n');
    load('./lispboot/test', text => {
      vconsole.clear();
      scalisp.runTests(text);
    });
  }

  function loadExample(filename) {
    vconsole.clear();
    vconsole.out('loading ' + filename + '\n');
    load('./lispboot/examples/' + filename, text => {
      vconsole.clear();
      $input.value = text;
    });
  }

  vconsole.out('loading bootProgram code...\n');
  load('./lispboot/boot.lisp', bootCode => {
    bootProgram = scalisp.parse(bootCode);

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
  xhr.onreadystatechange = () => {
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
