const vid = document.querySelector('video');
navigator.mediaDevices.getUserMedia({video: true}) // request cam
.then(stream => {
  vid.srcObject = stream; // don't use createObjectURL(MediaStream)
  return vid.play(); // returns a Promise
})
.then(()=>{ // enable the button
  const btn = document.getElementById('snap');
  btn.disabled = false;
  btn.onclick = e => {
    takeASnap()
    .then(get_barcode);
  };
})
.catch(e=>console.log('please use the fiddle instead'));

function takeASnap(){
  const canvas = document.createElement('canvas'); // create a canvas
  const ctx = canvas.getContext('2d'); // get its context
  canvas.width = vid.videoWidth; // set its size to the one of the video
  canvas.height = vid.videoHeight;
  ctx.drawImage(vid, 0,0); // the video
  return new Promise((res, rej)=>{
    canvas.toBlob(res, 'image/jpeg'); // request a Blob from the canvas
  });
}

function get_barcode(blob) {
  data = new FormData()
  data.append('image', blob)
  var request = new XMLHttpRequest();
  request.open("POST", "http://127.0.0.1:5000/decode");
  request.send(data)
  request.onreadystatechange = function () {

     if (request.readyState == 4 && (request.status == 200)) {
        var barcode = request.responseText;
        var barcodesInput = $('#barcodes').val()
        if (barcodesInput) {
          barcodesInput += ', '
        }
        $('#barcodes').val(barcodesInput + barcode)
     } else if(request.readyState == 4 && request.status == 404) {
        alert('Code barre non détecté')
     }
}
}