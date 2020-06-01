unit KM_CampaignTypes;
{$I KaM_Remake.inc}
interface

type
  //Unique campaign identification, stored as 3 ANSI letters (TSK, TPR, etc)
  //3 bytes are used to avoid string types issues
  TKMCampaignId = array [0..2] of Byte;


implementation

end.
