document.querySelectorAll('pre.mermaid').forEach(function (pre) {
  var code = pre.querySelector('code');
  var div = document.createElement('div');
  div.className = 'mermaid';
  div.textContent = code ? code.textContent : pre.textContent;
  pre.replaceWith(div);
});
mermaid.run();
