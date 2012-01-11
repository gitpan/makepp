function lr( cur ) {
  cur.parentNode.parentNode.className = cur.parentNode.parentNode.className ? '' : 'right';
}

function nonav( cur ) {
  cur.parentNode.parentNode.className = 'none';
}

function roll( cur, out ) {
  var now = out ? 'fold' : 'unfold';
  var then = out ? 'unfold' : 'fold';
  var lis = cur.parentNode.parentNode.childNodes;
  for( var i = 1; i < lis.length; i++ )
    if( lis[i].className == now )
      lis[i].className = then;
}

function fold( cur ) {
  cur.className = (cur.className=='fold') ? 'unfold' : 'fold';
}