// -----------------------------------------------------------------------------
// -- Utilities

/**
** Select a random description to put in the donation box window from a list of
** pre-defined choices.
*/
function getDescription() {
  var l = [
    "Because you're awesome!",
    "Well, aren't you generous?",
    "*cry* We'll never forget this!",
    "We'll use this wisely. Promise.",
    "Money? Now that you mention it...",
    "Christmas has come early this year!",
  ];
  return l[Math.floor((Math.random()*l.length))];
}

/**
** Send a POST request to the charge endpoint URL with a set of parameters as a
** JSON body, with UTF-8 encoding. Invoke one of two return continuations
** depending on whether or not the request completed with a 200 response.
*/
function postCharge(params, goodk, badk) {
  var xhr;
  xhr = new XMLHttpRequest();

  xhr.onreadystatechange = function() {
    if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
      goodk();
    } else if (xhr.readyState == XMLHttpRequest.DONE) {
      badk(xhr.responseText);
    }
  }

  xhr.open('POST', 'https://donate-svc.cowsay.pw/charge', true);
  xhr.setRequestHeader('Content-Type', 'application/json;charset=utf-8');
  xhr.send(JSON.stringify(params));
}

/*
** Decodes an error response from the server as JSON, and returns the resulting
** message string.
*/
function decodeError(err) {
  var err;

  try {
    err = JSON.parse(responseText).message;
  } catch (x) {
    err = 'Unknown error. :(';
  }

  return err;
}

/*
** Report an error to the user, in case something goes wrong. This writes the
** error to console.log as well as updating the 'status-msg' div element.
*/
function onoes(s)
{
  var time = new Date().toUTCString();
  var str = '[' + time + '] ERROR: ' +  s;

  console.log(str);

  var msg = document.getElementById('status-msg');
  msg.innerHTML = str;

  var div = document.getElementById('status-div');
  div.style.opacity = '1';
  div.style.backgroundColor = 'red';
  setTimeout(function(){ div.style.display = ''; }, 600);

  return null;
}

/*
** Report success to the user. This writes the error to console.log as well as
** updating the 'status-msg' div element.
*/
function ogood(s)
{
  var time = new Date().toString();
  var str = '[' + time + '] OK: ' +  s;

  console.log(str);

  var msg = document.getElementById('status-msg');
  msg.innerHTML = str;

  var div = document.getElementById('status-div');
  div.style.opacity          = '1';
  div.style.backgroundColor = 'green';
  setTimeout(function(){ div.style.display = ''; }, 600);

  return null;
}

// -----------------------------------------------------------------------------
// -- Dealing with $$$

function disableDonateButton()
{
  document.getElementById('stripe-pay-button').disabled = true;
}

function enableDonateButton()
{
  document.getElementById('stripe-pay-button').disabled = false;
}

function validateAmnt()
{
  var amnt = document.getElementById('monies').value.trim();
  if (amnt.length == 0) return onoes('You must enter a valid amount of money.');

  var match = /^(\d*)(?:\.(\d{1,2}))?$/.exec(amnt);
  if (!match) return onoes("Invalid amount: " + amnt);

  // Normalize input. If there's no leading zero before the decimal, then
  // there's no dollars. If you put in '23.3' you want 2330, not 2303.
  var r1 = match[1]; var r2 = match[2];
  if (r1 == null || r1.length == 0) r1 = "0";
  if (r2 != null && r2.length == 1) r2 += "0";

  var dolla = parseInt(r1, 10),
      cents = parseInt(r2 || "0", 10);
      total = (dolla * 100) + cents;
  if (total < 50) return onoes('Minimum charge is 50 cents!');
  return total;
}

// -----------------------------------------------------------------------------
// -- Apple Pay

// Apple Pay setup
Stripe.setPublishableKey(stripe_pubkey);
Stripe.applePay.checkAvailability(function(avail) {
  if (avail) {
    document.getElementById('apple-pay-div').style.display    = 'block';
    document.getElementById('apple-pay-button').style.display = 'block';
  }
});

/*
** Invoked when the user clicks the Apple Pay button on a supported iOS/macOS
** device. This uses Stripe to open up the Apple Pay window and complete the
** donation flow.
*/
function applePay() {
  // TODO FIXME: Test and enable Apple Pay.
  onoes("Apple Pay currently doesn't work :(");
  return;

  var amnt = validateAmnt();
  if (amnt == null) return;

  // Set up payment object
  var payment = {
    countryCode: 'US',
    currencyCode: 'USD',
    total: {
      label: 'Haskell.org',
      amount: parseFloat(amnt / 100).toFixed(2)
    },
  };

  // Logging function
  var logErr = function (e) { onoes(e.message); }

  // POST function, invoked by the Apple Pay flow to call the server and
  // return an appropriate result.
  var doPost = function (result, completion) {
    var params = {};
    params.donationToken   = result.token.id;
    // TODO FIXME: rest of parameters!
    //params.donationEmail  = ... ;
    //params.donationAmount = ... ;

    var goodk = function () {
      ogood('Successfully charged -- thank you!');
      completion(ApplePaySession.STATUS_SUCCESS);
    }

    var badk  = function (err) {
      onoes('Payment failed. Reason: ' + decodeError(err));
      completion(ApplePaySession.STATUS_FAILURE);
    }

    postCharge(params, goodk, badk);
  }

  // Go
  var session = Stripe.applePay.buildSession(payment, doPost, logErr);
  session.begin();
}

// -----------------------------------------------------------------------------
// -- Stripe Checkout

/*
** Stripe Checkout handler object. Created with our specified configuration, and
** later invoked when the user wants to donate in order to complete the donation
** flow.
*/
var checkoutHandler = StripeCheckout.configure({
  key:             stripe_pubkey,
  name:            'Donate to Haskell.org',
  panelLabel:      'Donate {{amount}} USD',
  currency:        'usd',
  allowRememberMe: false,
  bitcoin:         true,
  token:           function(token) {
    var amnt = validateAmnt();
    if (amnt == null) return;

    // Amount is OK, disable the donate button for now. It'll be re-enabled
    // after the request to the server completes and returns a value.
    disableDonateButton();

    var params = {};
    params.donationAmount = amnt;
    params.donationEmail  = token.email;
    params.donationToken  = token.id;

    var goodk = function () {
      ogood('Successfully charged -- thank you!');
      enableDonateButton();
    };

    var badk = function (err) {
      onoes('Payment failed. Reason: ' + decodeError(err));
      enableDonateButton();
    };

    postCharge(params, goodk, badk);
  }
});

/*
** Invoked when the user clicks the donate button. Validates that the given
** input amount of money is OK, and then calls the Stripe Checkout handler to
** complete the remaining donation flow.
*/
function stripePay(e) {
  var amnt = validateAmnt();
  if (amnt == null) return;

  checkoutHandler.open({
    amount: amnt,
    description: getDescription()
  });
  e.preventDefault();
}

// -----------------------------------------------------------------------------
// -- Top level handlers

// Open Apple Pay when the Apple Pay button is clicked
document
  .getElementById('apple-pay-button')
  .addEventListener('click', applePay);

// Open Stripe Checkout when the donate button is clicked.
document
  .getElementById('stripe-pay-button')
  .addEventListener('click', stripePay);

// Close Checkout on page navigation.
window.addEventListener('popstate', function () { checkoutHandler.close(); });

// Hide alert when button is clicked
document
  .getElementById('status-x-btn')
  .addEventListener('click', function () {
    var div = document.getElementById('status-div');
    div.style.opacity = '0';
    setTimeout(function(){ div.style.display = 'none'; }, 600);
  });
