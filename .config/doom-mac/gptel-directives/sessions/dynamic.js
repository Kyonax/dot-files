let emailForm = document.getElementById('dy-email-capture');
let emailCtaBtn = emailForm.querySelector('.form-cta');
let emailBtnText = emailCtaBtn.querySelector('.btn-text');
let emailBtnLoader = emailCtaBtn.querySelector('.btn-loader');
let emailContentTop = document.querySelector('#dy-email-capture-wrap > div:first-child');
let emailContentBottom = document.querySelector('#dy-email-capture-wrap > div:last-child');
let emailFormWrap = document.getElementById('dy-email-capture-form-wrap');
let emailWrap = document.getElementById('dy-email-capture-wrap');
let emailThankYou = document.getElementById('dy-email-capture-thank-you');
let emailError = document.getElementById('dy-email-capture-error');
let smsForm = document.getElementById('dy-sms-capture');
let smsCtaBtn = smsForm.querySelector('.form-cta');
let smsBtnText = smsCtaBtn.querySelector('.btn-text');
let smsBtnLoader = smsCtaBtn.querySelector('.btn-loader');
let smsWrap = document.getElementById('dy-sms-capture-wrap');
let smsPhoneInput = document.getElementById('dy-sms-capture-phone-input');
let smsThankYou = document.getElementById('dy-sms-capture-thank-you');
let smsError = document.getElementById('dy-sms-capture-error');
let quizButtons = document.querySelectorAll('.quiz-btn');
let formSubmitted = false;
let selectedQuizOption = '';

function toggleLoader(button, text, loader, show) {
  button.disabled = show;
  if (show) {
    text.classList.add('hide');
    loader.classList.remove('hide');
  } else {
    text.classList.remove('hide');
    loader.classList.add('hide');
  }
}

function toggleQuizButtons(disable) {
  quizButtons.forEach(button => {
    button.disabled = disable;
  });
}

function replaceLegalTextWithLinks() {
  const termsURL = '${Terms and Conditions URL}';
  const privacyURL = '${Privacy Policy URL}';

  const emailBottomTextElem = document.getElementById('dy-email-capture-bottom-text');
  const smsBottomTextElem = document.getElementById('dy-sms-capture-bottom-text');

  const elementsToUpdate = [emailBottomTextElem, smsBottomTextElem];

  elementsToUpdate.forEach(elem => {
    if (elem && elem.innerHTML) {
      let content = elem.innerHTML;
      content = content.replace(
        /Terms and Conditions/g,
        '<a href="'+termsURL+'" target="_blank" rel="noopener noreferrer" aria-label="Read our Terms and Conditions">Terms and Conditions</a>'
      );
      content = content.replace(
        /Privacy Policy/g,
        '<a href="'+privacyURL+'" target="_blank" rel="noopener noreferrer" aria-label="Read our Privacy Policy">Privacy Policy</a>'
      );
      elem.innerHTML = content;
    }
  });
}

function transitionToSmsView() {
  emailWrap.classList.add('is-fading-out');
  setTimeout(() => {
    emailWrap.classList.add('hide');
    emailWrap.classList.remove('is-fading-out');
    smsWrap.classList.remove('hide');
  }, 300);
}

replaceLegalTextWithLinks();

DY.API('event', {
  name: 'Question Shown',
  properties: {
    event_category: 'Email Capture Quiz',
    timestamp: new Date().toISOString()
  }
});

quizButtons.forEach(button => {
  button.addEventListener('click', (event) => {
    quizButtons.forEach(btn => btn.classList.remove('selected'));
    event.currentTarget.classList.add('selected');

    selectedQuizOption = event.currentTarget.innerText;

    DY.API('event', {
      name: 'Question Answer Selected',
      properties: {
        event_category: 'Email Capture Quiz',
        selected_answer: selectedQuizOption,
        timestamp: new Date().toISOString()
      }
    });
  });
});

emailForm.addEventListener('submit', (event) => {
  event.preventDefault();
  if (formSubmitted) {
    return;
  }
  formSubmitted = true;
  toggleLoader(emailCtaBtn, emailBtnText, emailBtnLoader, true);
  toggleQuizButtons(true);

  let formData = new FormData(emailForm);
  let email = formData.get('email');
  let offerCode = '${Email Capture Offer Code}';

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
      setTimeout(() => { emailError.classList.add("hide"); }, 2000);
      return;
    }

    emailContentTop.classList.add('hide');
    emailContentBottom.classList.add('hide');

    if (response?.data?.offerApplied) {
      emailThankYou.classList.remove("hide");
    }

    setTimeout(transitionToSmsView, 2000);

    DY.API('event', {
      name: 'Email Submitted Successfully',
      properties: {
        event_category: 'Email Capture Quiz',
        email_submitted: email,
        quiz_selection: selectedQuizOption
      }
    });

    if (response?.data?.customer?.cuid) {
      DY.API('event', {
        name: 'identify-v1',
        properties: { cuid: response.data.customer.cuid }
      });
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
    setTimeout(() => { emailError.classList.add("hide"); }, 2000);
  }).finally(() => {
      toggleLoader(emailCtaBtn, emailBtnText, emailBtnLoader, false);
      if (!formSubmitted) {
         toggleQuizButtons(false);
      }
    });
});

function formatPhoneNum(input) {
  var digits = input.replace(/\D/g, '');
  var format = '(???) ???-????';
  var international = false;
  var digitLen = Math.min(digits.length, 15);
  if (digits[0] == 1) { digitLen--; }
  if (digitLen > 10) {
    international = true;
    var countryCodeLen = Math.min(digitLen - 10, 3);
    var extraLen = 15 - digitLen;
    var countryCode = '';
    var i = 0;
    for (i = 0; i < countryCodeLen; i++) { countryCode += '?'; }
    format = countryCode + ' ??? ??? ?? ??';
    for (i = 0; i < extraLen; i++) { format += '?'; }
  }
  if (digits[0] == 1) { format = '? ' + format; }
  if (international) { format = '+' + format; }
  digits.split('').forEach(function(digit) { format = format.replace('?', digit); });
  return format.split('?')[0];
};

smsPhoneInput.addEventListener('input', (event) => {
  smsPhoneInput.value = formatPhoneNum(event.target.value);
});

smsForm.addEventListener('submit', (event) => {
  event.preventDefault();
  if (formSubmitted) { return; }
  formSubmitted = true;
  toggleLoader(smsCtaBtn, smsBtnText, smsBtnLoader, true);

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
    setTimeout(() => { smsError.classList.add("hide"); }, 2000);
  }).finally(() => {
      toggleLoader(smsCtaBtn, smsBtnText, smsBtnLoader, false);
    });
});
