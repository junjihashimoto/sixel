int
bufsize(int width,int height){
  return 256+((5+3*3+2+1)*width + 1)*height;
}

void putnum(unsigned char* buf,int* p, unsigned int r) {
  int v100 = (r)/100;
  int v10 = ((r)%100)/10;
  int v1 = (r)%10;
  if(v100)
    buf[(*p)++] = '0' + v100;
  if(v100 || v10)
    buf[(*p)++] = '0' + v10;
  buf[(*p)++] = '0' + v1;
}

int
img2sixel(unsigned char* buf, unsigned char* img,int width,int height){
  int i,j,k;
  int p=0;
  int c;
  int prev = (100 << 16 ) | (100 << 8 ) | 100;
  buf[p++] = 033;  // ESC
  buf[p++] = 'P';
  buf[p++] = '8';
  buf[p++] = ';';
  buf[p++] = '1';
  buf[p++] = ';';
  buf[p++] = '0';
  buf[p++] = 'q';
  buf[p++] = '"';
  buf[p++] = '1';
  buf[p++] = ';';
  buf[p++] = '1';
  buf[p++] = ';';
  putnum(buf,&p,width);
  buf[p++] = ';';
  putnum(buf,&p,height);
  buf[p++] = '#';
  buf[p++] = '0';
  buf[p++] = ';';
  buf[p++] = '2';
  buf[p++] = ';';
  putnum(buf,&p,100);
  buf[p++] = ';';
  putnum(buf,&p,100);
  buf[p++] = ';';
  putnum(buf,&p,100);
  buf[p++] = '#';
  buf[p++] = '0';
  for(j=0;j<height;j++){
    for(i=0;i<width;i++){
      int pix = (j*width+i)*3;
      int r = img[pix]*101/256;
      int g = img[pix+1]*101/256;
      int b = img[pix+2]*101/256;
      int cur = (r << 16) | (g << 8) | b;
      if (prev != cur){
	buf[p++] = '#';
	buf[p++] = '0';
	buf[p++] = ';';
	buf[p++] = '2';
	buf[p++] = ';';
	putnum(buf,&p,r);
	buf[p++] = ';';
	putnum(buf,&p,g);
	buf[p++] = ';';
	putnum(buf,&p,b);
      } else {
	int num;
	for(k=i+1;k<width;k++){
	  int pix = (j*width+k)*3;
	  int r = img[pix]*101/256;
	  int g = img[pix+1]*101/256;
	  int b = img[pix+2]*101/256;
	  int nxt = (r << 16) | (g << 8) | b;
	  if(nxt!=cur){
	    break;
	  }
	}
	num = (k-i)-1;
	if(num > 3){
	  buf[p++] = '!';
	  putnum(buf,&p,num+1);
	  i=k-1;
	}
      }
      buf[p++] = (1 << (j%6))+0x3f;
      prev = cur;
    }
    if(j%6 ==5){
      buf[p++] = '-'; // LR
    }else{
      buf[p++] = '$'; // CR
    }
  }
  buf[p++] = 033;  // END
  buf[p++] = '\\'; // END
  return p;
}

