unit KM_AITypes;
{$I KaM_Remake.inc}
interface

type
  //For now IDs must match with KaM
  TKMAIDefencePosType = (dtFrontLine, //Front line troops may not go on attacks, they are for defence
                         dtBackLine); //Back line troops may attack

  TKMFormation = record
    NumUnits, UnitsPerRow: Integer;
  end;

implementation

end.

