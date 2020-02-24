-module(rtsv2_slot_configuration).


-include_lib("id3as_common/include/common.hrl").


%% SlotTypes.purs is the authority for these

-type slot_configuration() ::
        #{ name := binary_string()
         , profiles := list(slot_profile())
         }.

-type slot_profile() ::
        #{ name := binary_string()
         , firstAudioSSRC := non_neg_integer()
         , firstVideoSSRC := non_neg_integer()
         }.


-export_type([ slot_configuration/0
             , slot_profile/0
             ]).
