unit KM_HandTypes;
interface
uses
  KM_ResFonts;

type
  TKMHandType = (
        hndHuman,
        hndComputer);

  TKMHandEntityType = (etNone, etUnit, etGroup, etHouse);

  TKMHandHouseLock = (hlNone, hlDefault, hlBlocked, hlGranted);

  TKMOverlayTextSettings = record
    WordWrap: Boolean;
    Font: TKMFont;
  end;

const
  HAND_NONE = -1; //No player
  HAND_ANIMAL = -2; //animals

implementation

end.
