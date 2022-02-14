unit KM_HandTypes;
interface

type
  TKMHandType = (
        hndHuman,
        hndComputer);

  TKMHandEntityType = (etNone, etUnit, etGroup, etHouse);

  TKMHandHouseLock = (hlDefault, hlBlocked, hlGranted);

const
  HAND_NONE = -1; //No player
  HAND_ANIMAL = -2; //animals

implementation

end.
