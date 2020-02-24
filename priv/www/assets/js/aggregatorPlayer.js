function update_state(allProfiles) {

  $.getJSON(".")
   .done(function(aggregatorPublicState) {
      if ($("#players").hasClass("inactive")) {
        $("#players").empty();
        $("#players").removeClass("inactive").addClass("active");
        setActiveContent(aggregatorPublicState);
      }

     var activeProfiles = $.map(aggregatorPublicState.activeStreamVariants, function(variant, index) { return variant.streamVariant; });

     $.each(aggregatorPublicState.streamDetails.slot.profiles, function(index, profile) {
       var id = '#' + profile.name;
       var last = $(id).children().last();
       if (activeProfiles.includes(profile.name)) {
         if (last[0].localName != "video") {
           last.replaceWith(videoTemplate(profile.name));
         }
       }
       else {
         if (last[0].localName != "h3") {
           last.replaceWith(noIngestTemplate());
         }
       }
     });
   })
    .fail(function(param) {
      if ($("#players").hasClass("active")) {
        $("#players").empty();
        $("#players").removeClass("active").addClass("inactive");
        $("#players").append("<span>no ingest</span>");
      }
    });
}

function videoTemplate(videoElementId) {
  return `<video id="${videoElementId}" autoplay controls muted></video>`;
}

function videoElementId(profileName) {
  return `ve-${profileName}`;
}

function noIngestTemplate() {
  return "<span>No Ingest</span>";
}

function setActiveContent(aggregatorPublicState) {

  $.each(aggregatorPublicState.streamDetails.slot.profiles, function(index, profile) {
    $("#players").append("<div id='" + profile.name + "' class='rtcVideo'><h3 class='ingestTitle'>" + profile.name + "<h3>" + noIngestTemplate() + "</div>")
  });

  $.each(aggregatorPublicState.activeStreamVariants, function(index, variant) {
    const id = videoElementId(variant.streamVariant);
    $('#' + variant.streamVariant).children().last().replaceWith(videoTemplate(id));
  });

  const pathname = window.location.pathname;
  const parent = pathname.substring(1, pathname.lastIndexOf("/"));
  const activeIngests = parent + "/activeIngests";

  $.each(aggregatorPublicState.activeStreamVariants, function(index, variant) {
    const id = videoElementId(variant.streamVariant);

    var config = {
      account: "",
      streamName: "",
      videoElementId: id,
      overrides: {
        socketAuthority: window.location.host,
        socketSecure: window.location.protocol === "https:",
        socketPath: `${activeIngests}/${variant.streamVariant}/session`,
      }
    };

    LimelightSDK.createPlayer(config);
  });
}

$(document).ready(function() {

  $.getJSON(".", function(aggregatorPublicState) {
    setActiveContent(aggregatorPublicState);

    var allProfiles = $.map(aggregatorPublicState.streamDetails.slot.profiles, function(profile, index) { return profile.name; });

    setInterval(function() {
      update_state(allProfiles);
    }, 500);

  });

})
