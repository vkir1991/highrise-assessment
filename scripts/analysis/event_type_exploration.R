# just playing around with the data, see what comes up, etc.

tmp <- readSQL("SELECT DISTINCT(event_type) FROM events")
tmp$event_type


# get a random event from each, just to see what the properties are
for (cur.event_type in sort(tmp$event_type)){
  cur.properties <- readSQL("SELECT * FROM event_properties WHERE event_id = (SELECT event_id FROM events WHERE event_type = '",cur.event_type,"' LIMIT 1)")
  print(cur.event_type)
  print(cur.properties)
  readline()
}

# tmp <- readSQL("SELECT * FROM event_properties WHERE event_id IN (SELECT event_id FROM events WHERE event_type = 'asdf' LIMIT 10)")
# tmp <- readSQL("SELECT * FROM event_properties a INNER JOIN events b on a.event_id = b.event_id WHERE b.event_type = 'asdf'")
# unique(tmp[!is.na(value)]$property)
# tmp[event_id==last(event_id) & !is.na(value)]


# [Experiment] Assignment
## don't use

# [Experiment] Exposure
## don't use

# [Singular] Install
## presumably initial install, not very useful here, all properties seem null anyway? double check this
## looked at 100 events, all values are empty, so don't use this

# Campaign_Finished
## just has a campaign_id, is this ad campaigns? without campaign table not very useful

# Closet_SavedCloset
## seems to have to do with saving a new outfit/look (though closet would imply broader storage?) - could be useful signal if you do this some minimum number of times

# Crew_CreatedCrew
## crews are basically clans, definitely a positive indicator if you create/join/etc. one

# Crew_DisbandedCrew
## not likely to matter

# Crew_InvitedUser
## invited someone to your crew, seems positive but correlated to creating or joining a crew, should bundle those together or at least check cor

# Crew_JoinedCrew
## see above, bundle with create

# Crew_LeftCrew
## probably negative, though plausibly you leave to join another, and you're really engaging with the ecosystem

# Crew_RequestedJoin
## see above; probably not very useful because we care more about whether or not you actually managed to join

# Crew_UpdatedCrewBio
## not likely to matter

# Crew_UpdatedCrewFlag
## not likely to matter

# Crew_UpdatedCrewName
## not likely to matter

# Crew_UpdatedCrewPrivacy
## not likely to matter

# Directory_ChangedDefaultContentLanguage
## not likely to matter

# Event_ClaimedCrewBonusReward
## probably positive (binary), but likely collinear with joining a crew and generically claiming an event reward (though not seeing an event for this)

# Event_SubmittedOutfit
## probably positive (binary), but would properly need to look at the negative as well: if there was an event running and you didn't submit, that's bad, but if there's no event that's fine

# FTUE_Completed
## FTUE = first-time user experience, so this depends on the specific thing you completed (look for ftue_name, goals_completed, goals_included)
## ftue_name is just "starter goals" in various languages, so doesn't seem very useful

# LuckyWheel_SpunLuckyWheel
## closer look needed, lots of properties here though most probably don't matter?
## spin_type: Free, VideoAd - this might matter, plausibly watching ads for more spins is useful, but going to hold off for expediency
## reward_type: Collectible, Lucky Tokens, Clothing, Grab, Bubbles, Gold, Custom Currency
## reward_amount: various numbers, makes sense for currencies
## depends how we want to handle this one; probably just number of spins or binary, and not worry about what you got out
## in a more sophisticated model, could try to see if getting better stuff early leads you to spend more, etc., but that's fairly complicated

# Marketplace_CompletedPurchase
## presumably marketplace is peer-to-peer, so not actually spending real money necessarily, so we can use this as a factor
## this is all in gold

# Marketplace_ListedItem
## specifics of what item you listed are probably not very important
## all in gold

# Messenger_ModifiedDMGroupMember
## not likely to matter

# Messenger_NamedGroupDM
## not likely to matter

# OOTD_SavedPhoto
## seems weak, but maybe?

# OOTD_Submitted
## probably a positive indicator

# PaymentFailed
## not likely to matter; more importantly it's not a good predictor because at this point you've already tried spending so it's circular

# Quest_Finished
## check if any values other than "quest_id" are non-empty; otherwise just treat as binary
## campaign_id and progress, both not very helpful, so just treat as binary

# Registration_CompletedSurveyEvent
## look more into it, but not likely to matter

# Registration_AddedNewRegistration
## not sure what you're registering, not likely to matter

# Registration_ChangedDefaultOutfit
## presumably avatar customization on new account creation, so maybe a very weak predictor that you're more bought in?

# Registration_ChoseGender
## necessary step I think; so doing it doesn't matter, but chosen gender might be predictive as a proxy for actual demographic; one-hot encode this

# Registration_FinishedDressingAvatar
## not very important; presumably not finishing the onboarding flow is a bad sign for spending, but that's so far up the funnel that we actually don't want to dilute things here
## do a filter on actually finishing registration, and only care about people who did. if you didn't, that's an onboarding issue that should be handled there

# Registration_FinishedRegistration
## see above
## time_spent could be useful, but it's NA for every single event anyway

# Registration_FirstLaunch
## see above

# Registration_LoggedIn
## see above

# Registration_PickedBirthday
## age is very likely to be predictive, even if it's just self-reported
## will need to poke around with data, because it's not going to be a linear variable, and likely just need to define some bands

# Registration_PickedStarterRoom
## see above

# Registration_PickedUsername
## see above

# Registration_RandomizedAvatar
## similar to changing default avatar; might matter but definitely even weaker than changeddefaultoutfit

# Registration_SeenIntroScreen
## this has time_since_startup (presumably seconds) so maybe useful as proxy for how long it took to register? depends where intro screen is

# Registration_SubmittedForgotPassword
## not likely to matter

# Registration_WentBack
## not likely to matter

# ReturnPopup_Submitted
## no idea what this is; return to home from a room? can check types of answers to get a sense of what this is, and inactivity duration
## answer: checking in, other, friends, ad; not much context here

# revenue_amount
## is this any spend? there's a 99.99 price for first one so possibly, explore more
## redundant with other measures of players spending money

# session_end
## if this is, in fact, a correct flag for a user session ending, then we can back out how long a session was and figure out first session length for each person which is huge
## seems to be, and seems to work

# session_start
## see above; not sure why this has price, productid, etc. properties

# Settings_SetPassword
## not likely to matter

# Settings_UserLoggedOut
## not likely to matter

# Share_OpenedDeepLink
## check type; maybe useful because it means someone sent you a deep link, or you clicked on notifications
## type = unknown, room, sale, user, world
## type could matter in theory, also not clear if these are from notifications but probably likely? for now just treat as binary

# Share_SharedOOTD
## looks useful, means you're sort of bought into the ecosystem

# Share_SharedProfile
## similarly useful

# Share_SharedRoom
## ditto

# Share_SharedScreenshot
## ditto - tie all shares into a single factor? maybe we care about total number of shares?

# Shop_BoughtGoldBar
## this is a conversion of gold into pure flex, so this almost certainly comes after you've spent real money anyway

# Shop_BoughtIAP
## this is our output!

# Shop_BoughtItem
## check unit_price vs usd_price; if you're buying stuff for in-game currency, that's probably a good sign that you're invested and will spend USD once you're out
## looks like this is all bubbles/gold (check currency property)

# Shop_BoughtItemOffer
## similar unit_price/usd_price thing, not sure how this differs from above
## also all bubbles/gold/custom currency, presumably these are limited-time offers, so maybe extra weight?

# Shop_SaleImpression
## need to poke around more, no idea what this really means

# Shop_SoldGoldBar
## ditto above gold bar, not likely to matter, just check if this ever really happens before spending real cash

# Shop_SpunGacha
## this seems to refer to grabs
## useful properties: completed_on_spin, completion_pct, spin_count (can pay for multiple spins at a time)

# Shop_WatchedAd
## watched an ad to get extra currency, probably a good sign? though conversely maybe people who try hard to get free currency won't actually spend

# SocialLink_Linked
## linking socials (x, tiktok, insta, snap) to your account
## positive sign, but binary, don't really care which socials you've linked

# SubscriptionEnded
## highrise premium or whatever it's called? at this point you've spent money

# SubscriptionRenewed
## you spent money!

# SubscriptionRequestedCancel
## post-spending money

# SubscriptionStarted
## this counts as spending money!

# Subscription_Transferred
## you've already spent money at this point

# SubscriptionUncanceled
## you've already spent money at this point

# SubscriptionUpgraded
## you've already spent money at this point

# Tipping_ClaimedTipJar
## collected money from a tip jar that you set up, presumably?
## always gold

# Tipping_SentTip
## might be a positive sign

# TS_All_User_Ban
## have to guess at what TS is, but presumably some chat service, in which case not very useful

# TS_Antispam_Mute
## not likely to matter

# TS_Community_Sift_Flagged_Content
## not likely to matter

# TS_Hive_Flagged_Content
## not likely to matter

# TS_NewUser_LowTrustScore
## not likely to matter

# TS_Persona_Verification_Approved
## not likely to matter

# TS_Persona_Verification_Expired
## not likely to matter

# TS_Persona_Verification_Started
## not likely to matter

# TS_Persona_Verification_Submitted
## not likely to matter

# TS_User_Mute
## this seems to be if YOU were muted, so not very useful as a signal (yeah, presumably you won't spend after a ban, but that's not predictively interesting)

# TS_User_Report
## similar to above

# TS_User_Warn
## similar to above

# UA_CreatedUser
## no relevant properties, so unclear what this really is, just initial login?

# UA_PurchasedFirstIAP
## this seems like the exact thing we want, need to cross-reference against some of the other paid things we've seen
## no relevant properties, so no way to check what was bought

# UA_PurchasedIAP
## double check that this is always either redundant or strictly after FirstIAP

# UA_RequestedIDFA
## not likely to matter

# UA_ScreenshotTaken
## why is this in user acquisition? otherwise not likely to matter

# unverified_revenue
## since we have a revenue_amount event, i'm guessing this is just a temporary pending state, which isn't useful

# VoiceCall_ParticipationEnded
## not likely to matter

# VW_FinishedVisit
## seems like this is a visit to a room? not sure why VW. check vw_type (room, etc.), time_spent_minutes


