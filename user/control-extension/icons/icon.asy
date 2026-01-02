draw((-1cm,-1cm),white);
draw((1cm,1cm),white);

fill(scale(1cm)*((-0.5,-0.1)--(0.5,-0.1)--(0.0,-0.8)--cycle),opacity(0.3)+red);
fill(scale(1cm)*((-0.5,+0.1)--(0.5,+0.1)--(0.0,+0.8)--cycle),opacity(0.3)+red);

draw(scale(1cm)*((-0.5, 0.6)--(-0.9, 0.0)--(-0.5,-0.6)) ,linewidth(0.7mm));
draw(scale(1cm)*((+0.5, 0.6)--(+0.9, 0.0)--(+0.5,-0.6)) ,linewidth(0.7mm));
draw(scale(1cm)*((+0.2, 0.6)--(-0.2,-0.6)) ,linewidth(0.7mm));

