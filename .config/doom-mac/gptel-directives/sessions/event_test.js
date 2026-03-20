let emailForm = document.getElementById('dy-email-capture');
let emailFormWrap = document.getElementById('dy-email-capture-form-wrap');
let emailWrap = document.getElementById('dy-email-capture-wrap');
let emailThankYou = document.getElementById('dy-email-capture-thank-you');
let emailError = document.getElementById('dy-email-capture-error');
let smsForm = document.getElementById('dy-sms-capture');
let smsWrap = document.getElementById('dy-sms-capture-wrap');
let smsPhoneInput = document.getElementById('dy-sms-capture-phone-input');
let smsThankYou = document.getElementById('dy-sms-capture-thank-you');
let smsError = document.getElementById('dy-sms-capture-error');
let formSubmitted = false;
const closeButton = document.querySelector(".dytmpl-close-icon");

closeButton.setAttribute("tabindex", "0");
emailForm.addEventListener('submit', (event) => {
  event.preventDefault();
  if (formSubmitted) {
    return;
  }
  formSubmitted = true;

  let formData = new FormData(emailForm);
  let email = formData.get('email');
  let offerCode = '${Email Capture Offer Code}';

  console.log('offerCode: ', offerCode);

  window.createCustomerFromEmailCapture({
    email: email,
    email_source: 'dy-email-capture-modal',
    offerCode: offerCode,
    isBookingOfferCode: false,
    skipPassword: true
  }).then((response) => {
    formSubmitted = false;
    if (response?.data?.userAlreadyExists) {
      emailError.classList.remove("hide");
      setTimeout(() => {
        emailError.classList.add("hide");
      }, 2000);
      return;
    }

    emailFormWrap.classList.add("hide");
    if (response?.data?.offerApplied) {
      emailThankYou.classList.remove("hide");
    }

    setTimeout(() => {
      emailWrap.classList.add("hide");
      smsWrap.classList.remove("hide");
    }, 2000);
  }).catch((err) => {
    console.log('err: ', err);
    formSubmitted = false;
    emailError.classList.remove("hide");
    setTimeout(() => {
      emailError.classList.add("hide");
    }, 2000);
  });
});

function formatPhoneNum(input) {
  var digits = input.replace(/\D/g, '');
  var format = '(???) ???-????';
  var international = false;

  var digitLen = Math.min(digits.length, 15);
  if (digits[0] == 1) {
    digitLen--;
  }

  if (digitLen > 10) {
    international = true;
    var countryCodeLen = Math.min(digitLen - 10, 3);
    var extraLen = 15 - digitLen;
    var countryCode = '';
    var i = 0;
    for (i = 0; i < countryCodeLen; i++) {
      countryCode += '?';
    }

    format = countryCode + ' ??? ??? ?? ??';
    for (i = 0; i < extraLen; i++) {
      format += '?';
    }
  }

  if (digits[0] == 1) {
    format = '? ' + format;
  }

  if (international) {
    format = '+' + format;
  }

  digits.split('').forEach(function(digit) {
    format = format.replace('?', digit);
  });

  return format.split('?')[0];
};

smsPhoneInput.addEventListener('input', (event) => {
  smsPhoneInput.value = formatPhoneNum(event.target.value);
});

smsForm.addEventListener('submit', (event) => {
  event.preventDefault();
  if (formSubmitted) {
    return;
  }
  formSubmitted = true;
  let formData = new FormData(smsForm);
  let phone = formData.get('phone');

  addFirstTimeVisitorPhoneAndOffer({
    phone: phone,
    offerCode: '${SMS Capture Offer Code}'
  }).then((response) => {
    smsForm.classList.add("hide");
    if (response.data.offerApplied) {
      smsThankYou.classList.remove("hide");
    }
  }).catch((err) => {
    console.log('err: ', err);
    formSubmitted = false;
    let errCode = err?.response?.data?.code;

    if (['USER_ALREADY_EXISTS', 'PHONE_ALREADY_SUBSCRIBED'].includes(errCode)) {
      smsError.innerHTML = err?.response?.data?.message;
    }

    smsError.classList.remove("hide");
    setTimeout(() => {
      smsError.classList.add("hide");
    }, 2000);
  });
});
