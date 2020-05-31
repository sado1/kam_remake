{
NavMesh - army vector field
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_NavMeshArmyVectorField;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes,
  KM_Points, KM_UnitGroup, KM_Houses, KM_ResHouses,
  KM_NavMeshFloodFill;

type

  TKMCombatStatus = (csNeutral = 0, csDefending, csAttackingCity, csAttackingEverything);
  TKMCombatStatusArray = array[0..MAX_HANDS] of TKMCombatStatus;

  TKMInfluenceInfo = record
    ClusterID, AllyInfluence, EnemyInfluence: Byte;
    Distance, Mark: Word;
  end;
  TKMInflInfoArray2 = array of TKMInfluenceInfo;

  TKMAllianceInfo2 = record
    // GroupsNumber = real number of groups, GroupsCount = count of groups in arrays
    GroupsCount, HousesCount, GroupsNumber, OwnersCount: Word;
    Groups: TKMUnitGroupArray;
    Houses: TKMHouseArray;
    GroupsPoly, HousesPoly: TKMWordArray;
  end;

  // Combat cluster
  TKMCombatCluster = record
    ReferenceID, GroupsCount, HousesCount: Word;
    Groups, Houses: TKMWordArray;
  end;
  TKMCombatClusters = record
    Count, UnreferencedCount: Word;
    Clusters: array of TKMCombatCluster;
  end;
  pTKMCombatCluster = ^TKMCombatCluster;
  // Evaluation of cluster (it is separated from TKMCombatCluster because of merging clusters in Flood Fill)
  TKMGroupCounterWeight = record
    Idx: Word;
    Group: TKMUnitGroup;
    TargetPosition: TKMPointDir;
    Status: Boolean;
  end;
  TKMCombatClusterThreat = record
    AttackingCity: Boolean;
    ClusterIdx: Word;
    BestDist, Threat, ThreatNearby: Single;
    Cluster: pTKMCombatCluster;
    CenterPoint: TKMPoint;
    Owners: TKMHandIDArray;
    CounterWeight: record
      InPlace, AtAdvantage, Ambushed: Boolean;
      GroupsCount, InPositionCnt, NearEnemyCnt: Word;
      Opportunity, InPositionStrength: Single;
      WeightedCount: array[TKMGroupType] of Word;
      Groups: array of TKMGroupCounterWeight
    end;
  end;
  TKMCombatClusterThreatArray = array of TKMCombatClusterThreat;

  {$IFDEF DEBUG_ArmyVectorField}
    TDebugArmyVectorField = array of record //fDbgVector
      Enemy: TKMAllianceInfo2;
      Ally: TKMAllianceInfo2;
      Alliance: TKMHandIDArray;
      Clusters: TKMCombatClusters;
      CCT: TKMCombatClusterThreatArray;
      ClustersMapp: TKMWordArray;
    end;
  {$ENDIF}

  TKMFindClusters = class;
  TArmyVectorField = class;

  TKMFindClusters = class(TNavMeshFloodFill)
  private
  protected
    fClusters: TKMCombatClusters;
    fInflInfo: TKMInflInfoArray2;

    procedure InitQueue(var aEnemy: TKMAllianceInfo2); reintroduce;
    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aClusterID: Word; const aDistance: Cardinal; const aPoint: TKMPoint); reintroduce;
    procedure ExpandPolygon(aIdx: Word; aCanBeExpanded: Boolean);
    procedure Flood(var aEnemy: TKMAllianceInfo2); reintroduce;
  public
    function FindClusters(var aClusters: TKMCombatClusters; var aAllianceInfo: TKMAllianceInfo2; var aDefInfo: TKMInflInfoArray2; var aQueueArray: TPolygonsQueueArr): Boolean;
  end;


  TArmyVectorField = class(TNavMeshFloodFill)
  private
  protected
    fOwner: TKMHandID;
    fOwners: TKMHandIDArray;
    fCntAllyPoly, fCntEnemyPoly: Word;
    fInflInfo: TKMInflInfoArray2;
    fFindClusters: TKMFindClusters;
    fClusters: TKMCombatClusters;
    {$IFDEF DEBUG_ArmyVectorField}
      fDebugCounter: Integer;
      fDebugDefPolyCnt: Integer;
      fDbgVector: TDebugArmyVectorField;
    {$ENDIF}

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MakeNewQueue(); override;
    function IsVisited(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx: Word; const aDistance: Cardinal; const aPoint: TKMPoint); override;
    procedure InitQueue(const aMaxIdx: Integer; var aInitIdxArray: TKMWordArray); override;
    procedure Flood(); override;

    function GetInitPolygonsGroups(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo2): Boolean;
    function GetInitPolygonsHouses(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo2; aOnlyCompleted: Boolean = True): Boolean;

    procedure InitClusterEvaluation();

    {$IFDEF DEBUG_ArmyVectorField}
      function GetAllianceIdxFromDebugArray(): Integer;
      procedure CopyVariablesForDebug1();
      procedure CopyVariablesForDebug2();
      function GenerateColorWSeed(aIdx: Integer): Cardinal;
      function GetCCTIdxFromGroup(aG: TKMUnitGroup): Integer;
    {$ENDIF}
  public
    Enemy: TKMAllianceInfo2;
    Ally: TKMAllianceInfo2;
    CCT: TKMCombatClusterThreatArray;

    constructor Create(aSorted: Boolean = True); override;
    destructor Destroy; override;

    property Clusters: TKMCombatClusters read fClusters;
    property QueueArray: TPolygonsQueueArr read fQueueArray;
    property Influence: TKMInflInfoArray2 read fInflInfo;

    function DetectEnemyPresence(var aOwners: TKMHandIDArray): Boolean;
    procedure DivideForces(aCS: TKMCombatStatus; var aCSA: TKMCombatStatusArray);
    procedure FindPositions();

    function LogStatus(): UnicodeString;
    procedure Paint(var aOwners: TKMHandIDArray; var aTargetGroups: TKMUnitGroupArray; var aTargetHouses: TKMHouseArray);

  end;


const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $7700FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
  MAX_CLUSTER_DISTANCE = 10;


implementation
uses
  SysUtils,
  KM_Hand, KM_HandsCollection,
  KM_AIFields, KM_NavMesh, KM_AIParameters,
  {$IFDEF DEBUG_ArmyVectorField}
  DateUtils,
  {$ENDIF}
  KM_CommonUtils,
  KM_RenderAux;




procedure DrawPolygon(aIdx: Integer; aOpacity: Byte; aFillColor: Cardinal; aOffset: Single = 0; aText: String = '');
var
  P0,P1,P2: TKMPoint;
begin
  if (aOpacity = 0) then
    Exit;
  with gAIFields.NavMesh do
  begin
    P0 := Nodes[ Polygons[aIdx].Indices[0] ];
    P1 := Nodes[ Polygons[aIdx].Indices[1] ];
    P2 := Nodes[ Polygons[aIdx].Indices[2] ];
    gRenderAux.TriangleOnTerrain(P0.X,P0.Y, P1.X,P1.Y, P2.X,P2.Y, aFillColor OR (aOpacity shl 24));
    if (Length(aText) > 0) then
      gRenderAux.Text(Polygons[aIdx].CenterPoint.X, Polygons[aIdx].CenterPoint.Y + aOffset, aText, $FFFFFFFF);
  end;
end;




{ TKMFindClusters }
function TKMFindClusters.CanBeExpanded(const aIdx: Word): Boolean;
begin
  //Result := fInflInfo[aIdx].Distance > 0;
  //Result := fInflInfo[aIdx].EnemyInfluence > 0;
  Result := fQueueArray[aIdx].Distance < MAX_CLUSTER_DISTANCE;
end;


procedure TKMFindClusters.MarkAsVisited(const aIdx, aClusterID: Word; const aDistance: Cardinal; const aPoint: TKMPoint);
begin
  inherited MarkAsVisited(aIdx, aDistance, aPoint);
  fInflInfo[aIdx].ClusterID := aClusterID;
end;


procedure TKMFindClusters.InitQueue(var aEnemy: TKMAllianceInfo2);
  function CreateNewCluster(aPolyIdx: Word): Integer;
  begin
    Result := fClusters.Count;
    MarkAsVisited(aPolyIdx, fClusters.Count, 0, gAIFields.NavMesh.Polygons[aPolyIdx].CenterPoint);
    InsertAndSort(aPolyIdx);
    with fClusters.Clusters[Result] do
    begin
      ReferenceID := Result; // Reference to itself
      GroupsCount := 0;
      HousesCount := 0;
      SetLength(Groups,4); // Create new array
      SetLength(Houses,4); // Create new array
    end;
    Inc(fClusters.Count);
  end;
  procedure AddWordField(aFied: Integer; var aCount: Word; var aArr: TKMWordArray); inline;
  begin
    if (Length(aArr) <= aCount) then
      SetLength(aArr,Length(aArr) + 4);
    aArr[ aCount ] := aFied;
    aCount := aCount + 1;
  end;
var
  K, PolyIdx, ClusterIdx: Integer;
begin
  fVisitedIdx := 1;
  fQueueCnt := 0;
  fClusters.Count := 0;
  if (Length(fClusters.Clusters) < aEnemy.GroupsCount + aEnemy.HousesCount) then
    SetLength(fClusters.Clusters, aEnemy.GroupsCount + aEnemy.HousesCount);
  // Copy groups
  for K := 0 to aEnemy.GroupsCount - 1 do
  begin
    PolyIdx := aEnemy.GroupsPoly[K];
    if IsVisited(PolyIdx) then
      ClusterIdx := fInflInfo[PolyIdx].ClusterID
    else
      ClusterIdx := CreateNewCluster(PolyIdx);
    with fClusters.Clusters[ClusterIdx] do
      AddWordField(K, GroupsCount, Groups);
  end;
  // Copy houses
  for K := 0 to aEnemy.HousesCount - 1 do
  begin
    PolyIdx := aEnemy.HousesPoly[K];
    if IsVisited(PolyIdx) then
      ClusterIdx := fInflInfo[PolyIdx].ClusterID
    else
      ClusterIdx := CreateNewCluster(PolyIdx);
    with fClusters.Clusters[ClusterIdx] do
      AddWordField(K, HousesCount, Houses);
  end;
end;


procedure TKMFindClusters.ExpandPolygon(aIdx: Word; aCanBeExpanded: Boolean);
var
  K, L, NearbyIdx, RefID, NearbyRefID: Integer;
begin
  RefID := fClusters.Clusters[ fInflInfo[aIdx].ClusterID ].ReferenceID;
  for K := 0 to gAIFields.NavMesh.Polygons[aIdx].NearbyCount-1 do
  begin
    NearbyIdx := gAIFields.NavMesh.Polygons[aIdx].Nearby[K];
    // Expand polygon
    if not IsVisited(NearbyIdx) then
    begin
      if aCanBeExpanded then
      begin
        MarkAsVisited( NearbyIdx, RefID,
                       fQueueArray[aIdx].Distance + KMDistanceWalk(fQueueArray[aIdx].DistPoint, gAIFields.NavMesh.Polygons[aIdx].NearbyPoints[K]),
                       gAIFields.NavMesh.Polygons[aIdx].NearbyPoints[K]);
        InsertAndSort(NearbyIdx);
      end;
    end
    // Merge clusters by changing reference (faster than copying arrays)
    else
    begin
      NearbyRefID := fClusters.Clusters[ fInflInfo[NearbyIdx].ClusterID ].ReferenceID;
      if (RefID <> NearbyRefID) then
        for L := 0 to fClusters.Count - 1 do
          if (fClusters.Clusters[L].ReferenceID = NearbyRefID) then
            fClusters.Clusters[L].ReferenceID := RefID;
    end;
  end;
end;


procedure TKMFindClusters.Flood(var aEnemy: TKMAllianceInfo2);
var
  Idx: Word;
begin
  InitQueue(aEnemy);
  if (fQueueCnt <= 0) then
    Exit;

  // Start merging clusters
  Idx := fStartQueue;
  while RemoveFromQueue(Idx) do
    ExpandPolygon(Idx, CanBeExpanded(Idx));
end;


function TKMFindClusters.FindClusters(var aClusters: TKMCombatClusters; var aAllianceInfo: TKMAllianceInfo2; var aDefInfo: TKMInflInfoArray2; var aQueueArray: TPolygonsQueueArr): Boolean;
var
  K,L,Cnt: Integer;
begin
  // Check groups and houses
  if (aAllianceInfo.GroupsCount = 0) AND (aAllianceInfo.HousesCount = 0) then
    Exit(False);
  // Copy variables
  fInflInfo := aDefInfo;
  fQueueArray := aQueueArray; // This is clear array with zeros
  // Find clusters
  Flood(aAllianceInfo);
  // Merge clusters (do it just once instead in every polygon)
  fClusters.UnreferencedCount := 0;
  for K := 0 to fClusters.Count - 1 do
    if (K = fClusters.Clusters[K].ReferenceID) then
    begin
      Inc(fClusters.UnreferencedCount);
      for L := 0 to fClusters.Count - 1 do
        if (K <> L) AND (fClusters.Clusters[L].ReferenceID = fClusters.Clusters[K].ReferenceID) then
        begin
          // Copy groups
          Cnt := fClusters.Clusters[K].GroupsCount;
          if (fClusters.Clusters[L].GroupsCount > 0) then
          begin
            Inc(fClusters.Clusters[K].GroupsCount, fClusters.Clusters[L].GroupsCount);
            if (Length(fClusters.Clusters[K].Groups) < fClusters.Clusters[K].GroupsCount) then
              SetLength(fClusters.Clusters[K].Groups, fClusters.Clusters[K].GroupsCount + 16);
            Move(fClusters.Clusters[L].Groups[0], fClusters.Clusters[K].Groups[Cnt], SizeOf(fClusters.Clusters[K].Groups[0]) * fClusters.Clusters[L].GroupsCount);
          end;
          // Copy houses
          Cnt := fClusters.Clusters[K].HousesCount;
          if (fClusters.Clusters[L].HousesCount > 0) then
          begin
            Inc(fClusters.Clusters[K].HousesCount, fClusters.Clusters[L].HousesCount);
            if (Length(fClusters.Clusters[K].Houses) < fClusters.Clusters[K].HousesCount) then
              SetLength(fClusters.Clusters[K].Houses, fClusters.Clusters[K].HousesCount + 8);
            Move(fClusters.Clusters[L].Houses[0], fClusters.Clusters[K].Houses[Cnt], SizeOf(fClusters.Clusters[K].Houses[0]) * fClusters.Clusters[L].HousesCount);
          end;
        end;
    end;

  aClusters := fClusters;
  Result := (fClusters.Count > 0);
end;




{ TArmyVectorField }
constructor TArmyVectorField.Create(aSorted: Boolean = True);
begin
  inherited Create(aSorted);

  fFindClusters := TKMFindClusters.Create(aSorted);
  {$IFDEF DEBUG_ArmyVectorField}
  SetLength(fDbgVector, 0);
  {$ENDIF}
end;


destructor TArmyVectorField.Destroy();
begin
  fFindClusters.Free;

  inherited;
end;


// Prepare new Queue
procedure TArmyVectorField.MakeNewQueue();
begin
  // Check length
  if (Length(fQueueArray) < gAIFields.NavMesh.PolygonsCnt) then
  begin
    SetLength(fQueueArray, gAIFields.NavMesh.PolygonsCnt);
    SetLength(fInflInfo, gAIFields.NavMesh.PolygonsCnt);
  end;
  // Clear queue
  fQueueCnt := 0;
  ClearVisitIdx();
  // Distance will be changed so clear array
  FillChar(fInflInfo[0], SizeOf(fInflInfo[0]) * Length(fInflInfo), #0);
  {$IFDEF DEBUG_ArmyVectorField}
    fDebugCounter := 0;
    if (fDebugDefPolyCnt = 0) then
      fDebugDefPolyCnt := gAIFields.NavMesh.PolygonsCnt;
  {$ENDIF}
end;


function TArmyVectorField.CanBeExpanded(const aIdx: Word): Boolean;
{$IFDEF DEBUG_ArmyVectorField}
  var
    K, Idx: Integer;
{$ENDIF}
begin
  Result := inherited CanBeExpanded(aIdx);

  {$IFDEF DEBUG_ArmyVectorField}
    Inc(fDebugCounter);
    if OVERLAY_AI_SUPERVISOR_A AND (fDebugCounter = Round(DateUtils.MilliSecondsBetween(Now, 0) * 0.01) mod fDebugDefPolyCnt) then
    begin
      //DrawPolygon(aIdx, -1, Min(250,75), COLOR_BLUE, IntToStr(aIdx));
      Idx := fStartQueue;
      for K := 0 to fQueueCnt - 1 do
      begin
        //DrawPolygon(Idx, -1, Min(250,K + 75), COLOR_BLUE, IntToStr(K));
        Idx := fQueueArray[Idx].Next;
      end;
    end;
  {$ENDIF}
end;


function TArmyVectorField.IsVisited(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Visited >= fVisitedIdx); // Arrays are common so >= must be here
end;


procedure TArmyVectorField.MarkAsVisited(const aIdx: Word; const aDistance: Cardinal; const aPoint: TKMPoint);
begin
  //fInflInfo[aIdx].EnemyInfluence := Min(250,fInflInfo[aIdx].Distance);
  //fInflInfo[aIdx].AllyInfluence := gAIFields.Influences.GetAlliancePresence(fOwner, aIdx, atAlly);
  //Inc(fInflInfo[aIdx].Distance,aDistance);
  fInflInfo[aIdx].Distance := aDistance;
  //gAIFields.NavMesh.Polygons[aIdx].NearbyLineLength

  inherited MarkAsVisited(aIdx, aDistance, aPoint);
end;


procedure TArmyVectorField.InitQueue(const aMaxIdx: Integer; var aInitIdxArray: TKMWordArray);
const
  INIT_DISTANCE = 0;
var
  I, Idx: Word;
begin
  fVisitedIdx := 4;
  if (aMaxIdx >= 0) then
    for I := 0 to aMaxIdx do
    begin
      Idx := aInitIdxArray[I];
      if not IsVisited(Idx) then
      begin
        MarkAsVisited(Idx, INIT_DISTANCE, gAIFields.NavMesh.Polygons[ Idx ].CenterPoint);
        InsertInQueue(Idx);
      end;
    end;
end;


procedure TArmyVectorField.Flood();
var
  Idx: Word;
  K: Integer;
begin
  while RemoveFromQueue(Idx) do
    if CanBeExpanded(Idx) then
      with gAIFields.NavMesh.Polygons[Idx] do
        for K := 0 to NearbyCount-1 do
        begin
          if not IsVisited(Nearby[K]) then
          begin
            MarkAsVisited( Nearby[K],
                           fQueueArray[Idx].Distance + KMDistanceWalk(fQueueArray[Idx].DistPoint, NearbyPoints[K]) + (6-NearbyLineLength[K])*5,
                           NearbyPoints[K]);
            InsertAndSort(Nearby[K]);
          end;
        end;
end;


function TArmyVectorField.GetInitPolygonsGroups(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo2): Boolean;
  // Dont add the same polygon for 1 group twice
  function CheckIdenticalPolygons(aStartIdx: Integer): Word;
  var
    K: Integer;
  begin
    with aAlliance do
      for K := aStartIdx to GroupsCount - 1 do
        if (GroupsPoly[K] = GroupsPoly[GroupsCount]) then
          Exit(0);
    Result := 1;
  end;
var
  PL: TKMHandID;
  K, L, StartIdx: Integer;
  G: TKMUnitGroup;
begin
  with aAlliance do
  begin
    OwnersCount := 0;
    GroupsCount := 0;
    GroupsNumber := 0;
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = aAllianceType) AND ((aAllianceType <> atAlly) OR gHands[PL].AI.Setup.NewAI) then
      begin
        Inc(OwnersCount);
        for K := 0 to gHands[PL].UnitGroups.Count - 1 do
        begin
          G := gHands[PL].UnitGroups.Groups[K];
          if (G <> nil) AND not G.IsDead AND not KMSamePoint(KMPOINT_ZERO,G.Position) then
            //AND ((aAllianceType <> atAlly) OR gHands[PL].AI.ArmyManagement.AttackNew.IsGroupInAction(G)) then // Select only combat groups
          begin
            Inc(GroupsNumber);
            L := 0;
            StartIdx := GroupsCount;
            while (L < G.Count) do
            begin
              if (G.Members[L] <> nil) AND not G.Members[L].IsDeadOrDying then
              begin
                if (Length(GroupsPoly) <= GroupsCount) then
                begin
                  SetLength(GroupsPoly, GroupsCount + 20);
                  SetLength(Groups, Length(GroupsPoly));
                end;
                GroupsPoly[GroupsCount] := gAIFields.NavMesh.KMPoint2Polygon[ G.Members[L].CurrPosition ];
                Groups[GroupsCount] := G;
                Inc(GroupsCount,CheckIdenticalPolygons(StartIdx));
                L := L + 5;
              end
              else
                L := L + 1;
            end;
          end;
        end;
      end;
    SetLength(GroupsPoly, GroupsCount);
    SetLength(Groups, GroupsCount);
    Result := GroupsCount > 0;
  end;
end;


function TArmyVectorField.GetInitPolygonsHouses(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo2; aOnlyCompleted: Boolean = True): Boolean;
const
  SCAN_HOUSES: THouseTypeSet = [htBarracks, htStore, htSchool, htTownhall]; // htWatchTower
var
  PL: TKMHandID;
  K: Integer;
  H: TKMHouse;
begin
  with aAlliance do
  begin
    OwnersCount := 0;
    HousesCount := 0;
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = aAllianceType) then
      begin
        Inc(OwnersCount);
        for K := 0 to gHands[PL].Houses.Count - 1 do
        begin
          H := gHands[PL].Houses[K];
          if (H <> nil) AND not H.IsDestroyed AND (H.HouseType in SCAN_HOUSES) AND (not aOnlyCompleted OR H.IsComplete) then
          begin
            if (Length(Houses) <= HousesCount) then
            begin
              SetLength(HousesPoly, HousesCount + 10);
              SetLength(Houses, Length(HousesPoly));
            end;
            HousesPoly[HousesCount] := gAIFields.NavMesh.KMPoint2Polygon[ H.Entrance ];
            Houses[HousesCount] := H;
            Inc(HousesCount);
          end;
        end;
      end;
    SetLength(HousesPoly, HousesCount);
    SetLength(Houses, HousesCount);
    Result := HousesCount > 0;
  end;
end;


procedure TArmyVectorField.InitClusterEvaluation();
const
  INFLUENCE_THRESHOLD = 150;
var
  OwnerDetected: Boolean;
  K, L, M, Cnt, OwnersCnt: Integer;
  G: TKMUnitGroup;
begin
  SetLength(CCT, fClusters.UnreferencedCount);
  if (Length(CCT) <= 0) then
    Exit;
  FillChar(CCT[0], Length(CCT) * SizeOf(CCT[0]), #0);

  // Init CCT + check if clusters are in the hostile city
  Cnt := 0;
  for K := 0 to fClusters.Count - 1 do
    if (fClusters.Clusters[K].ReferenceID = K) then // Skip unreferenced clusters
    begin
      CCT[Cnt].ClusterIdx := K;
      CCT[Cnt].BestDist := 1E10;
      CCT[Cnt].Cluster := @fClusters.Clusters[K];
      SetLength(CCT[Cnt].Owners, Enemy.OwnersCount);
      OwnersCnt := 0;
      for L := 0 to fClusters.Clusters[K].GroupsCount - 1 do
      begin
        G := Enemy.Groups[ fClusters.Clusters[K].Groups[L] ];
        CCT[Cnt].CenterPoint := KMPointAdd(CCT[Cnt].CenterPoint,G.Position);
        CCT[Cnt].Threat := CCT[Cnt].Threat + G.Count;
        OwnerDetected := False;
        for M := 0 to OwnersCnt - 1 do
          OwnerDetected := OwnerDetected OR (CCT[Cnt].Owners[M] = G.Owner);
        if not OwnerDetected then
        begin
          CCT[Cnt].Owners[OwnersCnt] := G.Owner;
          Inc(OwnersCnt);
        end;
      end;
      for L := 0 to fClusters.Clusters[K].HousesCount - 1 do
        CCT[Cnt].CenterPoint := KMPointAdd(CCT[Cnt].CenterPoint,Enemy.Houses[ fClusters.Clusters[K].Houses[L] ].Position);
      SetLength(CCT[Cnt].Owners, OwnersCnt);
      CCT[Cnt].CenterPoint.X := Round(CCT[Cnt].CenterPoint.X / Max(1,fClusters.Clusters[K].GroupsCount + fClusters.Clusters[K].HousesCount));
      CCT[Cnt].CenterPoint.Y := Round(CCT[Cnt].CenterPoint.Y / Max(1,fClusters.Clusters[K].GroupsCount + fClusters.Clusters[K].HousesCount));
      Inc(Cnt);
    end;
end;


procedure TArmyVectorField.DivideForces(aCS: TKMCombatStatus; var aCSA: TKMCombatStatusArray);
var
  PowerLeft: Single;
  AssignedGroups: TBooleanArray;

  procedure AddGroup(aGroupIdx, aCCTIdx: Word);
  var
    K: Integer;
  begin
    // Mark as assigned
    for K := aGroupIdx to Ally.GroupsCount - 1 do
    begin
      AssignedGroups[K] := True;
      if (K < Ally.GroupsCount - 1) AND (Ally.Groups[K] <> Ally.Groups[K + 1]) then
        break;
    end;
    // Add group to array
    with CCT[aCCTIdx].CounterWeight do
    begin
      if (Length(Groups) <= GroupsCount) then
        SetLength(Groups, GroupsCount + 16);
      Groups[ GroupsCount ].Idx := aGroupIdx;
      Groups[ GroupsCount ].Group := Ally.Groups[aGroupIdx];
      Inc(GroupsCount);
    end;
    PowerLeft := PowerLeft - Ally.Groups[aGroupIdx].Count;
    CCT[aCCTIdx].CounterWeight.Opportunity := CCT[aCCTIdx].CounterWeight.Opportunity + Ally.Groups[aGroupIdx].Count;
  end;
const
  PRIO_1_MIN_DISTANCE = 20;
  GAIN_GROUPS_PER_A_HOUSE = 3;
  PENALIZATION_GROUPS_CNT = 100;
  OpportunityArr: array [TKMGroupType,TKMGroupType] of Single = (
  // gtMelee, gtAntiHorse, gtRanged, gtMounted
    (    1.0,         2.0,      3.0,       0.5), // gtMelee
    (    0.5,         1.0,      2.0,       4.0), // gtAntiHorse
    (    1.0,         2.0,      3.0,       0.5), // gtRanged
    (    1.0,         0.2,      5.0,       1.0)  // gtMounted
  );
var
  ClusterIdx: Word;
  K, L, M, BestCCTIdx, BestGIdx, Overflow, Overflow2: Integer;
  MaxThreat, Distance, BestDistance, Bid, BestBid, SoldiersCnt: Single;
  P: TKMPoint;
  Pf: TKMPointF;
  PL: TKMHandID;
  Distances: TSingleArray;
  G: TKMUnitGroup;
begin
  if (Ally.GroupsCount <= 0) then
    Exit;

  // 1. Prio = distance from enemy cluster
  SetLength(AssignedGroups, Ally.GroupsCount);
  FillChar(AssignedGroups[0], SizeOf(AssignedGroups[0]) * Length(AssignedGroups), #0);
  G := nil;
  PowerLeft := 0; // It will be filled within following loop
  for K := 0 to Ally.GroupsCount - 1 do
  begin
    PowerLeft := PowerLeft + Ally.Groups[K].Count;
    if (G <> Ally.Groups[K]) AND (fQueueArray[ Ally.GroupsPoly[K] ].Visited > 0) AND (fQueueArray[ Ally.GroupsPoly[K] ].Distance <= PRIO_1_MIN_DISTANCE) then // Ignore threat at this distance
    begin
      BestCCTIdx := fClusters.Clusters[ fInflInfo[ Ally.GroupsPoly[K] ].ClusterID ].ReferenceID;
      for L := Low(CCT) to High(CCT) do
        if (CCT[L].Cluster = @fClusters.Clusters[BestCCTIdx]) then
        begin
          AddGroup(K, L);
          break;
        end;
    end;
    G := Ally.Groups[K]; // Skip following group if it is the same group
  end;

  if (PowerLeft <= 0) then
    Exit;

  // 2. Prio = defend city
  Overflow := 0;
  repeat
    Inc(Overflow);
    // Loop over the cluster to find the worst threat (faster than sorting it)
    BestGIdx := -1;
    BestCCTIdx := -1;
    for K := 0 to Length(CCT) - 1 do
      if (CCT[K].AttackingCity) AND ((BestCCTIdx < 0) OR (CCT[BestCCTIdx].Threat - CCT[BestCCTIdx].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv] < CCT[K].Threat - CCT[K].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv])) then
        BestCCTIdx := K;
    // Find closest groups
    if (BestCCTIdx >= 0) AND (CCT[BestCCTIdx].AttackingCity) AND (CCT[BestCCTIdx].Threat > 0) then
    begin
      if (Length(Distances) < Ally.GroupsCount) then
        SetLength(Distances,Ally.GroupsCount);
      for K := 0 to Ally.GroupsCount - 1 do
        Distances[K] := KMDistanceSqr(CCT[BestCCTIdx].CenterPoint,Ally.Groups[K].Position);

      Overflow2 := 0;
      while (Overflow2 < 1000) AND (CCT[BestCCTIdx].Threat > CCT[BestCCTIdx].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv]) do
      begin
        Inc(Overflow2);
        BestGIdx := -1;
        for K := 0 to Ally.GroupsCount - 1 do
          if not AssignedGroups[K] AND ((BestGIdx < 0) OR (Distances[K] < Distances[BestGIdx])) then
            BestGIdx := K;
        if (BestGIdx < 0) then
          break;
        AddGroup(BestGIdx, BestCCTIdx);
      end;
    end;
  until (Overflow > 100) OR (BestCCTIdx = -1) OR (BestGIdx = -1);

  if (PowerLeft <= 0) then
    Exit;

  // 3. Prio - support ally in fight
  Overflow := 0;
  repeat
    Inc(Overflow);
    // Loop over the cluster to find the worst threat (faster than sorting it)
    BestGIdx := -1;
    BestCCTIdx := -1;
    for K := 0 to Length(CCT) - 1 do
      if (CCT[K].CounterWeight.Opportunity > 0) AND ((BestCCTIdx < 0) OR (CCT[BestCCTIdx].Threat - CCT[BestCCTIdx].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv] < CCT[K].Threat - CCT[K].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv])) then
        BestCCTIdx := K;
    // Find closest groups
    if (BestCCTIdx >= 0) then
    begin
      if (Length(Distances) < Ally.GroupsCount) then
        SetLength(Distances,Ally.GroupsCount);
      for K := 0 to Ally.GroupsCount - 1 do
        Distances[K] := KMDistanceSqr(CCT[BestCCTIdx].CenterPoint,Ally.Groups[K].Position);

      Overflow2 := 0;
      while (Overflow2 < 1000) AND (CCT[BestCCTIdx].Threat > CCT[BestCCTIdx].CounterWeight.Opportunity * AI_Par[ATTACK_ArmyVectorField_DivideForces_DefendCityAdv]) do
      begin
        Inc(Overflow2);
        BestGIdx := -1;
        for K := 0 to Ally.GroupsCount - 1 do
          if not AssignedGroups[K] AND ((BestGIdx < 0) OR (Distances[K] < Distances[BestGIdx])) then
            BestGIdx := K;
        if (BestGIdx < 0) then
          break;
        AddGroup(BestGIdx, BestCCTIdx);
      end;
    end;
  until (Overflow > 100) OR (BestCCTIdx = -1) OR (BestGIdx = -1);;

  if (PowerLeft <= 0) then
    Exit;

  // Find common enemy
  if (aCS in [csAttackingCity, csAttackingEverything]) then
  begin
    BestDistance := 1E10;
    for PL in fOwners do
    begin
      // Get center point of 1 player
      SoldiersCnt := 0;
      Pf := KMPOINTF_ZERO;
      for K := 0 to Ally.GroupsCount - 1 do
        with Ally.Groups[K] do
          if (Owner = PL) then
          begin
            SoldiersCnt := SoldiersCnt + Count; // Weighted average
            Pf.X := Pf.X + Position.X * Count;
            Pf.Y := Pf.Y + Position.Y * Count;
          end;
      if (SoldiersCnt = 0) then
        continue;
      P := KMPoint(  Round(Pf.X / SoldiersCnt), Round(Pf.Y / SoldiersCnt)  );
      // Target = city (clusters with houses) OR every cluster if there are no houses
      for K := Low(CCT) to High(CCT) do
        if (CCT[K].Cluster.HousesCount > 0) OR (Enemy.HousesCount = 0) then
        begin
          Distance := KMDistanceSqr(CCT[K].CenterPoint, P);
          if (Distance < BestDistance) then
          begin
            BestDistance := Distance;
            BestCCTIdx := K;
          end;
        end;
    end;
    // Exit if no cluster has been found
    if (BestDistance = 1E10) then
      Exit;
    // Assign groups
    for K := 0 to Ally.GroupsCount - 1 do
      if not AssignedGroups[K] then
        AddGroup(K, BestCCTIdx);
  end
  else
  begin
    // 4. Prio - Defend city
    for K := 0 to Ally.GroupsCount - 1 do
      if not AssignedGroups[K] then
      begin
        BestCCTIdx := 0;
        BestDistance := 1E10;
        P := Ally.Groups[K].Position;
        for L := 1 to Length(CCT) - 1 do
          if (CCT[L].AttackingCity) then
          begin
            Distance := KMDistanceSqr(CCT[L].CenterPoint,P);
            if (BestDistance > Distance) then
            begin
              BestDistance := Distance;
              BestCCTIdx := L;
            end;
          end;
        if (BestDistance < 1E10) then
          AddGroup(K, BestCCTIdx);
      end;
  end;
end;


procedure TArmyVectorField.FindPositions();
var
  BattleLineCnt: Word;
  BattleLine: TKMWordArray;

  procedure FindBattleLine(aInitIdx: Word);
  var
    K, Idx, NearbyIdx: Word;
  begin
    BattleLineCnt := 0;
    fQueueCnt := 0;
    fVisitedIdx := fVisitedIdx + 1;
    InsertInQueue(aInitIdx);
    fQueueArray[aInitIdx].Visited := fVisitedIdx;
    while RemoveFromQueue(Idx) do
      for K := 0 to gAIFields.NavMesh.Polygons[Idx].NearbyCount - 1 do
      begin
        NearbyIdx := gAIFields.NavMesh.Polygons[Idx].Nearby[K];
        if (fQueueArray[NearbyIdx].Visited = 1) then // 1 is mark for initial FF of enemy groups
        begin
          fQueueArray[NearbyIdx].Visited := fVisitedIdx;
          InsertInQueue(NearbyIdx);
        end
        else if (fQueueArray[NearbyIdx].Visited = 0) then
        begin
          fQueueArray[NearbyIdx].Visited := fVisitedIdx; // Mark as visited so the polygon is not added to BattleLine twice
          if (Length(BattleLine) <= BattleLineCnt) then
            SetLength(BattleLine, BattleLineCnt + 40);
          BattleLine[BattleLineCnt] := NearbyIdx;
          Inc(BattleLineCnt);
        end;
      end;
  end;

  procedure AssignPositions(aIdx: Integer);
  const
    SQR_IN_PLACE_TOLERANCE = 15*15;
  var
    NearEnemy: Boolean;
    K, L, BestIdx: Integer;
    BestDistance, Distance: Single;
    G: TKMUnitGroup;
  begin
    BestIdx := 0;
    for K := 0 to CCT[aIdx].CounterWeight.GroupsCount - 1 do
    begin
      G := CCT[aIdx].CounterWeight.Groups[K].Group;
      BestDistance := 1E10;
      for L := 0 to BattleLineCnt - 1 do
      begin
        Distance := KMDistanceSqr(gAIFields.NavMesh.Polygons[ BattleLine[L] ].CenterPoint,G.Position);
        if (Distance < BestDistance) then
        begin
          BestDistance := Distance;
          BestIdx := L;
        end;
      end;
      if (BestDistance < 1E10) then
      begin
        NearEnemy := (fQueueArray[  Ally.GroupsPoly[ CCT[aIdx].CounterWeight.Groups[K].Idx]  ].Visited > 0);
        CCT[aIdx].CounterWeight.Groups[K].TargetPosition.Loc := gAIFields.NavMesh.Polygons[ BattleLine[BestIdx] ].CenterPoint;
        CCT[aIdx].CounterWeight.Groups[K].TargetPosition.Dir := dirN;
        CCT[aIdx].CounterWeight.Groups[K].Status := (BestDistance < SQR_IN_PLACE_TOLERANCE) OR NearEnemy;
        if CCT[aIdx].CounterWeight.Groups[K].Status then
        begin
          Inc(CCT[aIdx].CounterWeight.InPositionCnt);
          Inc(CCT[aIdx].CounterWeight.NearEnemyCnt,Byte(NearEnemy));
          CCT[aIdx].CounterWeight.InPositionStrength := CCT[aIdx].CounterWeight.InPositionStrength + CCT[aIdx].CounterWeight.Groups[K].Group.Count;
        end;
      end;
    end;
  end;

var
  K, InitIdx: Integer;
begin
  {$IFDEF DEBUG_ArmyVectorField}
  // Array will be filled by Flood fill so debug must be saved now
  CopyVariablesForDebug1();
  {$ENDIF}

  if (Enemy.GroupsCount = 0) AND (Enemy.HousesCount = 0) then
    Exit;
  if (fVisitedIdx = 0) then
    fVisitedIdx := 1;
  for K := Low(CCT) to High(CCT) do
    if (CCT[K].CounterWeight.GroupsCount > 0) then
    begin
      if (CCT[K].Cluster.GroupsCount > 0) then
        InitIdx := Enemy.GroupsPoly[ CCT[K].Cluster.Groups[0] ]
      else
        InitIdx := Enemy.HousesPoly[ CCT[K].Cluster.Houses[0] ];
      FindBattleLine(InitIdx);
      AssignPositions(K);
    end;

  for K := Low(CCT) to High(CCT) do
    with CCT[K].CounterWeight do
    begin
      InPlace := InPositionCnt > GroupsCount * AI_Par[ATTACK_ArmyVectorField_EvalClusters_InPlace];
      AtAdvantage := InPositionStrength > CCT[K].Threat * AI_Par[ATTACK_ArmyVectorField_EvalClusters_AtAdvantage];
      Ambushed := NearEnemyCnt > GroupsCount * AI_Par[ATTACK_ArmyVectorField_EvalClusters_Ambushed];
    end;

  {$IFDEF DEBUG_ArmyVectorField}
  CopyVariablesForDebug2();
  {$ENDIF}
end;


function TArmyVectorField.DetectEnemyPresence(var aOwners: TKMHandIDArray): Boolean;
begin
  fOwners := aOwners;
  fOwner := aOwners[0];
  GetInitPolygonsGroups(atEnemy, Enemy);
  GetInitPolygonsHouses(atEnemy, Enemy);
  Result := (Enemy.GroupsCount > 0) OR (Enemy.HousesCount > 0);
  if Result then
  begin
    MakeNewQueue();
    fFindClusters.FindClusters(fClusters, Enemy, fInflInfo, fQueueArray);
    GetInitPolygonsGroups(atAlly, Ally);
    InitClusterEvaluation();
  end;
end;



{$IFDEF DEBUG_ArmyVectorField}
function TArmyVectorField.GetAllianceIdxFromDebugArray(): Integer;
var
  K, L: Integer;
begin
  Result := -1;
  if (Length(fOwners) <= 0) then
    Exit;

  for K := 0 to Length(fDbgVector) - 1 do
    for L := 0 to Length(fDbgVector[K].Alliance) - 1 do
      if (fOwner = fDbgVector[K].Alliance[L]) then
        Result := K;

  if (Result = -1) then
  begin
    Result := Length(fDbgVector);
    SetLength(fDbgVector, Length(fDbgVector) + 1);
  end;
end;


procedure TArmyVectorField.CopyVariablesForDebug1();
var
  K, Team: Integer;
begin
  Team := GetAllianceIdxFromDebugArray();
  if (Team = -1) then
    Exit;

  with fDbgVector[Team] do
  begin
    if (Length(ClustersMapp) <> Length(fInflInfo)) then
      SetLength(ClustersMapp, Length(fInflInfo));
    for K := Low(ClustersMapp) to High(ClustersMapp) do
      if (fQueueArray[K].Visited = 1) then
        ClustersMapp[K] := fInflInfo[K].ClusterID
      else
        ClustersMapp[K] := High(Word);
  end;
end;


procedure TArmyVectorField.CopyVariablesForDebug2();
  procedure CopyAlliance(var aFrom: TKMAllianceInfo2; var aTo: TKMAllianceInfo2);
  begin
    aTo.GroupsCount  := aFrom.GroupsCount;
    aTo.HousesCount  := aFrom.HousesCount;
    aTo.GroupsNumber := aFrom.GroupsNumber;
    aTo.OwnersCount  := aFrom.OwnersCount;
    SetLength(aTo.Groups,     aTo.GroupsCount);
    SetLength(aTo.Houses,     aTo.HousesCount);
    SetLength(aTo.GroupsPoly, aTo.GroupsCount);
    SetLength(aTo.HousesPoly, aTo.HousesCount);
    if (aTo.GroupsCount > 0) then Move(aFrom.Groups[0],     aTo.Groups[0],     SizeOf(aTo.Groups[0])     * aTo.GroupsCount);
    if (aTo.HousesCount > 0) then Move(aFrom.Houses[0],     aTo.Houses[0],     SizeOf(aTo.Houses[0])     * aTo.HousesCount);
    if (aTo.GroupsCount > 0) then Move(aFrom.GroupsPoly[0], aTo.GroupsPoly[0], SizeOf(aTo.GroupsPoly[0]) * aTo.GroupsCount);
    if (aTo.HousesCount > 0) then Move(aFrom.HousesPoly[0], aTo.HousesPoly[0], SizeOf(aTo.HousesPoly[0]) * aTo.HousesCount);
  end;
var
  K, Team: Integer;
begin
  Team := GetAllianceIdxFromDebugArray();
  if (Team = -1) then
    Exit;

  CopyAlliance(Ally,  fDbgVector[Team].Ally);
  CopyAlliance(Enemy, fDbgVector[Team].Enemy);

  SetLength(fDbgVector[Team].Alliance, Length(fOwners));
  Move(fOwners[0], fDbgVector[Team].Alliance[0], SizeOf(fOwners[0])*Length(fOwners));

  fDbgVector[Team].Clusters.Count := fClusters.Count;
  SetLength(fDbgVector[Team].Clusters.Clusters, fClusters.Count);
  for K := 0 to fClusters.Count - 1 do
    with fDbgVector[Team].Clusters.Clusters[K] do
    begin
      ReferenceID := fClusters.Clusters[K].ReferenceID;
      GroupsCount := fClusters.Clusters[K].GroupsCount;
      HousesCount := fClusters.Clusters[K].HousesCount;
      SetLength(Groups,GroupsCount);
      SetLength(Houses,HousesCount);
      if (GroupsCount > 0) then Move(fClusters.Clusters[K].Groups[0], Groups[0], SizeOf(Groups[0])*GroupsCount);
      if (HousesCount > 0) then Move(fClusters.Clusters[K].Houses[0], Houses[0], SizeOf(Houses[0])*HousesCount);
    end;
  //{
  SetLength(fDbgVector[Team].CCT, Length(CCT));
  for K := Low(CCT) to High(CCT) do
    with fDbgVector[Team].CCT[K] do
    begin
      AttackingCity      := CCT[K].AttackingCity;
      ClusterIdx         := CCT[K].ClusterIdx;
      Cluster            := @fDbgVector[Team].Clusters.Clusters[ClusterIdx];
      BestDist           := CCT[K].BestDist;
      Threat             := CCT[K].Threat;
      ThreatNearby       := CCT[K].ThreatNearby;
      CenterPoint        := CCT[K].CenterPoint;
      SetLength(Owners, Length(CCT[K].Owners));
      if (Length(Owners) > 0) then
        Move(CCT[K].Owners[0], Owners[0], SizeOf(Owners[0]) * Length(Owners));
      CounterWeight.GroupsCount        := CCT[K].CounterWeight.GroupsCount;
      CounterWeight.InPlace            := CCT[K].CounterWeight.InPlace;
      CounterWeight.AtAdvantage        := CCT[K].CounterWeight.AtAdvantage;
      CounterWeight.Ambushed           := CCT[K].CounterWeight.Ambushed;
      CounterWeight.GroupsCount        := CCT[K].CounterWeight.GroupsCount;
      CounterWeight.InPositionCnt      := CCT[K].CounterWeight.InPositionCnt;
      CounterWeight.NearEnemyCnt       := CCT[K].CounterWeight.NearEnemyCnt;
      CounterWeight.Opportunity        := CCT[K].CounterWeight.Opportunity;
      CounterWeight.InPositionStrength := CCT[K].CounterWeight.InPositionStrength;
      CounterWeight.WeightedCount[TKMGroupType(0)] := CCT[K].CounterWeight.WeightedCount[TKMGroupType(0)];
      CounterWeight.WeightedCount[TKMGroupType(2)] := CCT[K].CounterWeight.WeightedCount[TKMGroupType(1)];
      CounterWeight.WeightedCount[TKMGroupType(2)] := CCT[K].CounterWeight.WeightedCount[TKMGroupType(2)];
      CounterWeight.WeightedCount[TKMGroupType(3)] := CCT[K].CounterWeight.WeightedCount[TKMGroupType(3)];
      SetLength(CounterWeight.Groups, CounterWeight.GroupsCount);
      if (CounterWeight.GroupsCount > 0) then
        Move(CCT[K].CounterWeight.Groups[0], CounterWeight.Groups[0], SizeOf(TKMGroupCounterWeight) * CounterWeight.GroupsCount);
    end;
    //}
end;
{$ENDIF}


function TArmyVectorField.LogStatus(): UnicodeString;
{$IFDEF DEBUG_ArmyVectorField}
const
  STR_COLOR_WHITE = '[$FFFFFF]';
var
  Team, K, Idx: Integer;
  Color: Cardinal;
{$ENDIF}
begin
  {$IFDEF DEBUG_ArmyVectorField}
  fOwner := gMySpectator.HandID;
  Team := GetAllianceIdxFromDebugArray();
  if (Team = -1) OR (Length(CCT) = 0) then
    Exit('');

  Result := '|Army vector field:';

  Idx := -1;
  if (gMySpectator.Selected is TKMUnitGroup) then
    Idx := GetCCTIdxFromGroup(TKMUnitGroup(gMySpectator.Selected));

  for K := Low(fDbgVector[Team].CCT) to High(fDbgVector[Team].CCT) do
    with fDbgVector[Team].CCT[K] do
    begin
      Color := GenerateColorWSeed(ClusterIdx) AND $FFFFFF;
      Result := Format('%s|%d. [$%s]###%s:',[Result, K, IntToHex(Color,6), STR_COLOR_WHITE]);
      Result := Format('%s Threat %.0f/%.0f..%2.0f%%;',[Result, ThreatNearby, Threat, ThreatNearby/Max(1,Threat/100)]);
      Result := Format('%s Opportunity (%d): %.0f/%.0f..%2.0f%%;',[Result, CounterWeight.InPositionCnt, CounterWeight.InPositionStrength, CounterWeight.Opportunity, CounterWeight.InPositionStrength / Max(1,CounterWeight.Opportunity/100)]);
      Result := Format('%s [$%s]Attacking City%s,',[Result, IntToHex(COLOR_RED * Byte(AttackingCity            ) + $FFFFFF * Byte(not AttackingCity            ),6), STR_COLOR_WHITE]);
      Result := Format('%s [$%s]Place%s,',         [Result, IntToHex(COLOR_RED * Byte(CounterWeight.InPlace    ) + $FFFFFF * Byte(not CounterWeight.InPlace    ),6), STR_COLOR_WHITE]);
      Result := Format('%s [$%s]Advantage%s,',     [Result, IntToHex(COLOR_RED * Byte(CounterWeight.AtAdvantage) + $FFFFFF * Byte(not CounterWeight.AtAdvantage),6), STR_COLOR_WHITE]);
      Result := Format('%s [$%s]Ambush%s',         [Result, IntToHex(COLOR_RED * Byte(CounterWeight.Ambushed   ) + $FFFFFF * Byte(not CounterWeight.Ambushed   ),6), STR_COLOR_WHITE]);
      if (K = Idx) then
        Result := Format('%s <<<<<<',[Result]);
    end;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;


{$IFDEF DEBUG_ArmyVectorField}
function TArmyVectorField.GenerateColorWSeed(aIdx: Integer): Cardinal;
begin
  aIdx := Max(aIdx,0) + 1; // Seed > 0
  Result := GetRandomColorWSeed( KaMRandomWSeed(aIdx,High(Integer)) ); // Use random number so colors are more different if indexes are close to each other
end;
{$ENDIF}


{$IFDEF DEBUG_ArmyVectorField}
function TArmyVectorField.GetCCTIdxFromGroup(aG: TKMUnitGroup): Integer;
  function IsGroupInAlliance(var A: TKMAllianceInfo2): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    for K := 0 to A.GroupsCount - 1 do
      if (aG = A.Groups[K]) then
        Exit(True);
  end;
  function FindGroupInCCT(var aIdx: Integer): Boolean;
  var
    K, L: Integer;
  begin
    Result := False;
    for K := Low(CCT) to High(CCT) do
      for L := 0 to CCT[K].CounterWeight.GroupsCount - 1 do
        if (CCT[K].CounterWeight.Groups[L].Group = aG) then
        begin
          aIdx := K;
          Exit(True);
        end;
  end;
  function FindGroupInCluster(var aIdx: Integer): Boolean;
  var
    K,L: Integer;
  begin
    Result := False;
    for K := Low(CCT) to High(CCT) do
      for L := 0 to CCT[K].Cluster.GroupsCount - 1 do
        if (Enemy.Groups[ CCT[K].Cluster.Groups[L] ] = aG) then
        begin
          aIdx := K;
          Exit(True);
        end;
  end;
begin
  Result := -1;
  if IsGroupInAlliance(Ally) then
    FindGroupInCCT(Result)
  else if IsGroupInAlliance(Enemy) then
    FindGroupInCluster(Result);
end;
{$ENDIF}


procedure TArmyVectorField.Paint(var aOwners: TKMHandIDArray; var aTargetGroups: TKMUnitGroupArray; var aTargetHouses: TKMHouseArray);
{$IFDEF DEBUG_ArmyVectorField}
  function FindGroup(aG: TKMUnitGroup): Boolean;
  var
    K: Integer;
    PL: TKMHandID;
  begin
    for PL := 0 to gHands.Count - 1 do
      for K := 0 to gHands[PL].UnitGroups.Count - 1 do
        if (aG = gHands[PL].UnitGroups.Groups[K]) then
          Exit(True);
    Result := False;
  end;
  function FindHouse(aH: TKMHouse): Boolean;
  var
    K: Integer;
    PL: TKMHandID;
  begin
    for PL := 0 to gHands.Count - 1 do
      for K := 0 to gHands[PL].Houses.Count - 1 do
        if (aH = gHands[PL].Houses[K]) then
          Exit(True);
    Result := False;
  end;
var
  K, L, Team, SelectedIdx: Integer;
  Color, Opacity: Cardinal;
  G: TKMUnitGroup;
  H: TKMHouse;
  //PL: TKMHandID;
  //P: TKMPoint;
{$ENDIF}
begin
  {$IFDEF DEBUG_ArmyVectorField}
  fOwner := gMySpectator.HandID;
  Team := GetAllianceIdxFromDebugArray();

  if (Team = -1) then
    Exit;

  SelectedIdx := -1;
  if (gMySpectator.Selected is TKMUnitGroup) then
    SelectedIdx := GetCCTIdxFromGroup(TKMUnitGroup(gMySpectator.Selected));

  // Groups and houses in cluster
  {
  for K := 0 to fDbgVector[Team].Clusters.Count - 1 do
    if (fDbgVector[Team].Clusters.Clusters[K].ReferenceID = K) then
    begin
      Color := $99000000 OR ($00FFFFFF AND GenerateColorWSeed(K));
      for L := 0 to fDbgVector[Team].Clusters.Clusters[K].GroupsCount - 1 do
      begin
        G := fDbgVector[Team].Enemy.Groups[ fDbgVector[Team].Clusters.Clusters[K].Groups[L] ];
        if FindGroup(G) then
          gRenderAux.CircleOnTerrain(G.Position.X, G.Position.Y, 1, Color, Color);
      end;
      for L := 0 to fDbgVector[Team].Clusters.Clusters[K].HousesCount - 1 do
      begin
        H := fDbgVector[Team].Enemy.Houses[ fDbgVector[Team].Clusters.Clusters[K].Houses[L] ];
        if FindHouse(H) then
          gRenderAux.CircleOnTerrain(H.Position.X, H.Position.Y, 1, Color, Color);
      end;
    end;
  //}

  // Polygon in cluster
  with fDbgVector[Team] do
    for K := 0 to Min(High(ClustersMapp),gAIFields.NavMesh.PolygonsCnt - 1) do
      if (ClustersMapp[K] <> High(Word)) then
      begin
        L := Clusters.Clusters[ ClustersMapp[K] ].ReferenceID;
        Color := $00FFFFFF AND GenerateColorWSeed(L);
        DrawPolygon(K, $22, $44000000 OR Color);
      end;

  // Target of cluster
  for K := Low(fDbgVector[Team].CCT) to High(fDbgVector[Team].CCT) do
  begin
    Color := $00FFFFFF AND GenerateColorWSeed(fDbgVector[Team].CCT[K].ClusterIdx);
    Opacity := $88000000;
    if (K = SelectedIdx) then
    begin
      Opacity := $BB000000;
      with fDbgVector[Team].CCT[K] do
        gRenderAux.CircleOnTerrain(CenterPoint.X, CenterPoint.Y, 1, Opacity OR Color, $FF000000 OR COLOR_BLACK);
    end;
    with fDbgVector[Team].CCT[K].Cluster^ do
    begin
      for L := 0 to GroupsCount - 1 do
      begin
        G := fDbgVector[Team].Enemy.Groups[ Groups[L] ];
        if FindGroup(G) then
          gRenderAux.CircleOnTerrain(G.Position.X, G.Position.Y, 1, Opacity OR Color, $FF000000 OR COLOR_RED);
      end;
      for L := 0 to HousesCount - 1 do
      begin
        H := fDbgVector[Team].Enemy.Houses[ Houses[L] ];
        if FindHouse(H) then
          gRenderAux.CircleOnTerrain(H.Position.X, H.Position.Y, 1, Opacity OR Color, $FF000000 OR COLOR_BLUE);
      end;
    end;
    with fDbgVector[Team].CCT[K].CounterWeight do
      for L := 0 to GroupsCount - 1 do
      begin
        gRenderAux.Quad(Groups[L].TargetPosition.Loc.X, Groups[L].TargetPosition.Loc.Y, ($99000000 - Byte(Groups[L].Status)*$44000000) OR Color);
        G := Groups[L].Group;
        if FindGroup(G) then
        begin
          gRenderAux.Line(G.Position.X, G.Position.Y, Groups[L].TargetPosition.Loc.X, Groups[L].TargetPosition.Loc.Y, $FF000000 OR Color);
          gRenderAux.CircleOnTerrain(G.Position.X, G.Position.Y, 1, Opacity OR Color, $FF000000 OR COLOR_GREEN);
        end;
      end;

  end;

  {
  if GetInitPolygonsGroups(atEnemy, Enemy) then
  begin
    MakeNewQueue();
    fFindClusters.Paint(fClusters, Enemy, fInflInfo, fQueueArray);
    InitClusterEvaluation();
    for K := Low(CCT) to High(CCT) do
    begin
      Color := $66000000 OR ($00FFFFFF AND GetRandomColorWSeed((K+10)*99));
      for L := 0 to CCT[K].Cluster.GroupsCount - 1 do
      begin
        P := Enemy.Groups[ CCT[K].Cluster.Groups[L] ].Position;
        gRenderAux.CircleOnTerrain(P.X, P.Y, 1, Color, $FF000000 OR COLOR_RED);
      end;
      for L := 0 to CCT[K].Cluster.HousesCount - 1 do
      begin
        P := Enemy.Houses[ CCT[K].Cluster.Houses[L] ].Position;
        gRenderAux.CircleOnTerrain(P.X, P.Y, 1, Color, $FF000000 OR COLOR_BLUE);
      end;
    end;
    //DivideForces();
    //FindPositions();
  end;
  //}

  {$ENDIF}
end;

end.
