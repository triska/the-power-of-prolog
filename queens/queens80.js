var canvas = document.getElementsByTagName('canvas')[0];
var N = 80;
var ctx = canvas.getContext('2d');
var i = 0;

setInterval(function(){
    //ctx.clearRect(0,0,canvas.width,canvas.height);
    if (i < instrs.length) {
        var instr = instrs[i];

        i = i + 1;
        var r, g, b;

        switch(instr.i) {
        case "i": r = g = b = 170; break;
        case "q": r = g = b = 0; break;
        case "c": r = g = b = 255; break;
        }
        ctx.fillStyle = 'rgb('+r+','+g+','+b+')';
        var w=500/N,     h=canvas.height/N;
        var x=(instr.x-1)*(canvas.width/N),
            y=(N-instr.y)*(canvas.height/N);
        ctx.fillRect(x,y,w,h);
    }
}, 1);
