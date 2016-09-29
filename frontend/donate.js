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
** Send a POST request to a URL with a set of parameters as a JSON body with
** UTF-8 encoding. Invoke one of two return continuations depending on whether
** or not the request completed with a 200 response.
*/
function ajaxPost(url, params, goodk, badk) {
  var xhr;
  xhr = new XMLHttpRequest();

  xhr.onreadystatechange = function() {
    if (xhr.readyState == XMLHttpRequest.DONE && xhr.status == 200) {
      goodk();
    } else {
      badk(xhr.responseText);
    }
  }

  xhr.open("POST", url, true);
  xhr.setRequestHeader("Content-Type", "application/json;charset=utf-8");
  xhr.send(JSON.stringify(params));
}

function onoes(s)
{
  console.log("ERROR [donate.js]: " + s);
  /*
  $('#payment_status').removeClass('in'); // fade out if necessary
  $('#payment_status').removeClass('alert-success').addClass('alert-error');
  $('#status_message').html("<small>" + s + "</small>");
  $('#payment_status').addClass('in');
  */
  return null;
}

function ogood(s)
{
  console.log("OK [donate.js]: " + s);
  /*
  $('#payment_status').removeClass('in'); // fade out if necessary
  $('#payment_status').removeClass('alert-error').addClass('alert-success');
  $('#status_message').html("<small>" + s + "</small>");
  $('#payment_status').addClass('in');
  */
  return null;
}

// hide alert when button is clicked
/*
$(function() {
  $('#pay_status_close_btn').click(function () {
    $('#payment_status').removeClass('in');
  });
});
*/

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
// -- Entry point

// Apple Pay setup
Stripe.setPublishableKey(stripe_pubkey);
Stripe.applePay.checkAvailability(function(avail) {
  // TODO FIXME: Implement Apple Pay.
  if (false && avail) {
    document.getElementById('apple-pay-div').style.display    = 'block';
    document.getElementById('apple-pay-button').style.display = 'block';
  }
});

// Stripe Checkout setup
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

    // Amount is OK, disable the donate button
    disableDonateButton();

    // Construct the JSON object we'll send to the backend server.
    var params = {};
    params.donationAmount = amnt;
    params.donationEmail  = token.email;
    params.donationToken  = token.id;

    // The good continuation posts a happy message and re-enables the donation
    // button.
    var goodk = function () {
      var date = new Date();

      // Yay!
      ogood(date.toUTCString() + ': Successfully charged -- thank you!');
      enableDonateButton();
    };

    // The bad continuation posts an error and re-enables the donation button.
    var badk = function (responseText) {
      var date = new Date();
      var err;

      try {
        err = JSON.parse(responseText).message;
      } catch (x) {
        err = 'Unknown error. :(';
      }

      // Womp womp...
      onoes(date.toUTCString() + ': Payment failed. Reason: ' + err);
      enableDonateButton();
    };

    // Go
    ajaxPost('https://donate-svc.cowsay.pw/charge', params, goodk, badk);
  }
});

// Open checkout when the donate button is clicked.
document.getElementById('stripe-pay-button').addEventListener('click', function(e) {
  var amnt = validateAmnt();
  if (amnt == null) return;

  checkoutHandler.open({ amount: amnt, description: getDescription() });
  e.preventDefault();
});

// Close Checkout on page navigation.
window.addEventListener('popstate', function () {
  checkoutHandler.close();
});
