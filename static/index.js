"use strict";

if (typeof navigator.mediaDevices.getUserMedia !== 'function') {
    const err = new Error('Your browser can not use getUserMedia().');
    alert(`${err.name} ${err.message}`);
    throw err;
}

const flags = {
  ids: { video: 'video_area', capture: 'capture_image' },
  size: { width: 300, height: 300 }
};

const app = Elm.Main.init(
  { node: document.getElementById('main')
  , flags: flags
  }
);

const constraints = {
  audio: false,
  video: {...flags.size, facingMode: "environment" }
};

function handleSuccess(videoId, stream) {
  const video = document.getElementById(videoId);
  const videoTracks = stream.getVideoTracks();
  console.log('Got stream with constraints:', constraints);
  console.log(`Using video device: ${videoTracks[0].label}`);
  window.stream = stream; // make variable available to browser console
  video.srcObject = stream;
}

function handleError(error) {
  if (error.name === 'ConstraintNotSatisfiedError') {
    let v = constraints.video;
    errorMsg(`The resolution ${v.width.exact}x${v.height.exact} px is not supported by your device.`);
  } else if (error.name === 'PermissionDeniedError') {
    errorMsg('Permissions have not been granted to use your camera and ' +
      'microphone, you need to allow the page access to your devices in ' +
      'order for the demo to work.');
  }
  errorMsg(`getUserMedia error: ${error.name}`, error);
}

function errorMsg(msg, error) {
  const errorElement = document.getElementById('main');
  errorElement.innerHTML += `<div class="flash flash-error">${msg}</siv>`;
  if (typeof error !== 'undefined') {
    console.error(error);
  }
}

async function initCamera(videoId) {
  try {
    const stream = await navigator.mediaDevices.getUserMedia(constraints);
    handleSuccess(videoId, stream);
  } catch (e) {
    handleError(e);
  }
}

function captureImage(videoId, captureId) {
  var canvas = document.getElementById(captureId);
  var video = document.getElementById(videoId);
  canvas.width  = video.videoWidth;
  canvas.height = video.videoHeight;

  const ctx = canvas.getContext('2d');
  ctx.drawImage(video, 0, 0);
  return ctx.getImageData(0, 0, video.videoWidth, video.videoHeight);
}

app.ports.startCamera.subscribe(function() { initCamera(flags.ids.video) });

app.ports.captureImage.subscribe(function() {
  const imageData = captureImage(flags.ids.video, flags.ids.capture);
  const qrcode = jsQR(imageData.data, imageData.width, imageData.height)
  app.ports.updateQRCode.send(qrcode);
});
