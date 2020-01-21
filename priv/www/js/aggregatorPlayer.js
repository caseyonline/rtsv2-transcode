function update_state(allProfiles) {

  $.getJSON(".", function(aggregatorPublicState) {

    var activeProfiles = $.map(aggregatorPublicState.activeStreamVariants, function(variant, index) { return variant.streamVariant; });

    $.each(aggregatorPublicState.streamDetails.slot.profiles, function(index, profile) {
      var id = '#' + profile.streamName;
      var last = $(id).children().last();
      if (activeProfiles.includes(profile.streamName)) {
        if (last[0].localName != "rtc-video") {
          last.replaceWith(videoTemplate(profile.streamName));
        }
      }
      else {
        if (last[0].localName != "h3") {
          last.replaceWith(noIngestTemplate());
        }
      }
    });
  });
}

function videoTemplate(streamName) {
  var src = "/../activeIngests/" + streamName;
  return "<rtc-video src='" + src + "'></rtc-video>";
}

function noIngestTemplate() {
  return "<span>No Ingest</span>";
}

$(document).ready(function() {

  $.getJSON(".", function(aggregatorPublicState) {

    var allProfiles = $.map(aggregatorPublicState.streamDetails.slot.profiles, function(profile, index) { return profile.streamName; });

    $.each(aggregatorPublicState.streamDetails.slot.profiles, function(index, profile) {
      $("#players").append("<div id='" + profile.streamName + "' class='rtcVideo'><h3 class='ingestTitle'>" + profile.streamName + "<h3>" + noIngestTemplate() + "</div>")
    });

    $.each(aggregatorPublicState.activeStreamVariants, function(index, variant) {
      $('#' + variant.streamVariant).children().last().replaceWith(videoTemplate(variant.streamVariant));
    });

    setInterval(function() {
      update_state(allProfiles);
    }, 500);

  });

})
