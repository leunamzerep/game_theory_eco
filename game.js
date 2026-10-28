$(document).ready(function () {
    var step = 1;
    let error = false;
    var percievedValueBySeller;
    var topPriceBySeller;
    var proposedPriceBySeller;
    var percievedValueByBuyer;
    var topPriceByBuyer;
    var proposedPriceByBuyer;
    var averagePrice;
    var finalPrice;

    $(".next").on("click", function () {
        if ($(this).closest("a").length) {
            return;
        }
        error = false;
        if (step === 4 || step === 9) {

            let prefix = (step === 4) ? "Seller" : "Buyer";

            let countPercieved = $("#percievedValueBy" + prefix).val().replace(/\D/g, "").length;
            let countTop = $("#topPriceBy" + prefix).val().replace(/\D/g, "").length;
            let countProposed = $("#proposedPriceBy" + prefix).val().replace(/\D/g, "").length;

            if (countPercieved < 6) {
                error = true;
                $("#percievedValueBy" + prefix).next().show();
            } else {
                $("#percievedValueBy" + prefix).next().hide();
            }
            if (countTop < 6) {
                error = true;
                $("#topPriceBy" + prefix).next().show();
            } else {
                $("#topPriceBy" + prefix).next().hide();
            }
            if (countProposed < 6) {
                error = true;
                $("#proposedPriceBy" + prefix).next().show();
            } else {
                $("#proposedPriceBy" + prefix).next().hide();
            }
        } else if (step === 5 || step === 10) {
            let prefix = (step === 5) ? "seller" : "buyer";
            if (!($("#" + prefix + "ExpertTrue").prop("checked")) && !$("#" + prefix + "ExpertFalse").prop("checked")) {
                error = true;
                $("#" + prefix + "Expert").next().show();
            } else {
                $("#" + prefix + "Expert").next().hide();
            }
            if (!($("#" + prefix + "Male").prop("checked")) && !$("#" + prefix + "Female").prop("checked")) {
                error = true;
                $("#" + prefix + "Gender").next().show();
            } else {
                $("#" + prefix + "Gender").next().hide();
            }
            if ($("#" + prefix + "Age").val().length < 1) {
                error = true;
                $("#" + prefix + "Age").next().show();
            } else {
                $("#" + prefix + "Age").next().hide();
            }
        }

        if (error && (step === 4 || step === 9)) {
            alert("Por favor, complete todos los campos con 6 o 7 dígitos.");
            return;
        } else if (error && (step === 5 || step === 10)) {
            alert("Por favor, responde las preguntas.");
            return;
        } else {
            error = false;
            if (step == 10) {
                percievedValueBySeller = parseInt($("#percievedValueBySeller").val().replace(/\D/g, ""));
                topPriceBySeller = parseInt($("#topPriceBySeller").val().replace(/\D/g, ""));
                proposedPriceBySeller = parseInt($("#proposedPriceBySeller").val().replace(/\D/g, ""));
                percievedValueByBuyer = parseInt($("#percievedValueByBuyer").val().replace(/\D/g, ""));
                topPriceByBuyer = parseInt($("#topPriceByBuyer").val().replace(/\D/g, ""));
                proposedPriceByBuyer = parseInt($("#proposedPriceByBuyer").val().replace(/\D/g, ""));
                averagePrice = Math.round((topPriceBySeller + proposedPriceBySeller + topPriceByBuyer + proposedPriceByBuyer) / 4);

                sellerKnows = $("#sellerExpertTrue").prop("checked") ? 1 : 0;
                buyerKnows = $("#buyerExpertTrue").prop("checked") ? 1 : 0;
                sellerMale = $("#sellerMale").prop("checked") ? 1 : 0;
                buyerMale = $("#buyerMale").prop("checked") ? 1 : 0;
                sellerAge = parseInt($("#sellerAge").val());
                buyerAge = parseInt($("#buyerAge").val());

                let prices = [
                    averagePrice,
                    topPriceBySeller,
                    proposedPriceBySeller,
                    topPriceByBuyer,
                    proposedPriceByBuyer
                ];

                prices.sort((a, b) => a - b);

                prices.forEach((price, index) => {
                    let formatted = price.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ".");
                    $("#price" + (index + 1)).text(formatted + "$");
                });
            };
            $('#step' + step).hide();
            step++;
            $('#step' + step).show();

            if (step === 2) {
                $(".eyes").css("display", "flex");
                $(".sellerNotAllow").hide();
                $(".buyerAllow").hide();
            } else if (step === 6) {
                $(".buyerAllow").show();
                $(".buyerNotAllow").hide();
            } else if (step === 7) {
                $(".sellerNotAllow").show();
                $(".sellerAllow").hide();
            } else if (step === 11) {
                $(".sellerNotAllow").hide();
                $(".sellerAllow").show();
            } else if (step === 13) {
                $(".eyes").hide();
            }
        }
    });

    $("#sellerExpertTrue").on("change", function () {
        if ($("#sellerExpertFalse").prop("checked")) {
            $("#sellerExpertFalse").prop("checked", false);
        }
    });
    $("#sellerExpertFalse").on("change", function () {
        if ($("#sellerExpertTrue").prop("checked")) {
            $("#sellerExpertTrue").prop("checked", false);
        }
    });
    $("#buyerExpertTrue").on("change", function () {
        if ($("#buyerExpertFalse").prop("checked")) {
            $("#buyerExpertFalse").prop("checked", false);
        }
    });
    $("#buyerExpertFalse").on("change", function () {
        if ($("#buyerExpertTrue").prop("checked")) {
            $("#buyerExpertTrue").prop("checked", false);
        }
    });

    $("#sellerMale").on("change", function () {
        if ($("#sellerFemale").prop("checked")) {
            $("#sellerFemale").prop("checked", false);
        }
    });
    $("#sellerFemale").on("change", function () {
        if ($("#sellerMale").prop("checked")) {
            $("#sellerMale").prop("checked", false);
        }
    });
    $("#buyerMale").on("change", function () {
        if ($("#buyerFemale").prop("checked")) {
            $("#buyerFemale").prop("checked", false);
        }
    });
    $("#buyerFemale").on("change", function () {
        if ($("#buyerMale").prop("checked")) {
            $("#buyerMale").prop("checked", false);
        }
    });
    
    $(".prices").on("click", function () {

        if ($(this).text() !== "Abandonar negociación") {
            finalPrice = parseInt($(this).text().replace(/\D/g, ""));
        } else {
            finalPrice = "NN";
        }

        var payload = {
            sellerPercieved: percievedValueBySeller,
            sellerTop: topPriceBySeller,
            sellerProposed: proposedPriceBySeller,
            buyerPercieved: percievedValueByBuyer,
            buyerTop: topPriceByBuyer,
            buyerProposed: proposedPriceByBuyer,
            average: averagePrice,
            chosenPrice: finalPrice,
            sellerKnows: sellerKnows,
            buyerKnows: buyerKnows,
            sellerMale: sellerMale,
            buyerMale: buyerMale,
            sellerAge: sellerAge,
            buyerAge: buyerAge
        };

        fetch("https://script.google.com/macros/s/AKfycbzJZ9Haajfw5EUKom5rKWj5GW2oyQXp2Ss-jv91uRVVsO0jeakRbMyoZ6DX9B-GkpPGKQ/exec", {
            method: "POST",
            mode: "no-cors",
            headers: { "Content-Type": "application/x-www-form-urlencoded" },
            body: new URLSearchParams(payload)
        })
            .then(() => {
                console.log("Datos enviados!");
            })
            .catch(err => console.error("Error guardando:", err));

        $('#step' + step).hide();
        step++;
        $('#step' + step).show();
    });

    $(".moneyInput").on("focus", function () {
        if ($(this).val() === "$") {
            setCaret(this, 0);
        }
    });

    $(".moneyInput").on("input", function () {
        let val = $(this).val().replace(/\D/g, "");
        if (val.startsWith("0")) {
            val = val.replace(/^0+/, "");
        }
        val = val.slice(0, 7);
        let formatted = val.replace(/\B(?=(\d{3})+(?!\d))/g, ".");
        $(this).val(formatted + " $");
        setCaret(this, formatted.length);
    });

    $(".ageInput").on("input", function () {
        let val = $(this).val().replace(/\D/g, "");
        if (val.startsWith("0")) {
            val = val.replace(/^0+/, "");
        }
        val = val.slice(0, 2);
        $(this).val(val);
    });

    function setCaret(el, pos) {
        requestAnimationFrame(() => {
            el.setSelectionRange(pos, pos);
        });
    }

    $(".copyLink").on("click", function () {
        let url = window.location.href;
        navigator.clipboard.writeText(url).then(() => {
            alert("¡Link copiado al portapapeles!");
        }).catch(err => {
            console.error("Error al copiar: ", err);
        });
    });

    $(".restart").on("click", function () {
        location.reload();
    });
});
