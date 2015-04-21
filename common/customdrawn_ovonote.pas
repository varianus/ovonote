unit customdrawn_ovonote;

{$mode objfpc}{$H+}

interface

uses
  // RTL / FCL
  Classes, SysUtils, Types, Math, fpcanvas,
  // LazUtils
  lazutf8,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  // Others only for types
  StdCtrls, ComCtrls, Forms, ExtCtrls,
  //
  customdrawndrawers, customdrawn_common;

type
  { TCDDrawerOvoNote }

  TCDDrawerOvoNote = class(TCDDrawerCommon)
  private
  public
    function GetMeasures(AMeasureID: Integer): Integer; override;
    procedure CalculatePreferredSize(ADest: TCanvas; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;

    // General drawing routines
    // TCDRadioButton
    procedure DrawTickmark(ADest: TCanvas; ADestPos: TPoint;
      AState: TCDControlState); reintroduce;
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    procedure DrawCheckBox(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;

  end;

implementation

function TCDDrawerOvoNote.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
  //
  TCDCHECKBOX_SQUARE_HALF_HEIGHT: Result := Floor(GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT)/2);
  TCDCHECKBOX_SQUARE_HEIGHT: Result := DPIAdjustment(55);
  else
    result := Inherited  GetMeasures(AMeasureID);
  end;
end;

procedure TCDDrawerOvoNote.CalculatePreferredSize(ADest: TCanvas;
  AControlId: TCDControlID; AState: TCDControlState;
  AStateEx: TCDControlStateEx; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;

  case AControlId of
  // In the LCL TEdit AutoSizes only its Height, so follow this here
  cidEdit: PreferredHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx)+8;
  end;
end;

procedure TCDDrawerOvoNote.DrawTickmark(ADest: TCanvas; ADestPos: TPoint; AState: TCDControlState);
begin
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := Palette.BtnShadow;
  ADest.Pen.Width:= DPIAdjustment(2);
  ADest.Line(0, 0,  ADestPos.X, ADestPos.Y);
  ADest.Line(0, ADestPos.Y, ADestPos.X, 0);
end;


procedure TCDDrawerOvoNote.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lSquareHeight: Integer;
begin
  lSquareHeight := GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT);

  // the square background
  ADest.Pen.Style := psSolid;
  ADest.Pen.Width:= DPIAdjustment(4);
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := Palette.BtnShadow;

  ADest.Rectangle(Bounds(1, 1, ASize.cx, ASize.cy));

end;

procedure TCDDrawerOvoNote.DrawCheckBox(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lColor: TColor;
  lSquareHeight, lValue3: Integer;
  lTextHeight, lTextY: Integer;
begin

  DrawCheckBoxSquare(ADest, Point(0, 0), ASize, AState, AStateEx);

  if (csfOn in AState) or (csfPartiallyOn in AState) then
    DrawTickmark(ADest, Point(Asize.cx, ASize.cy), AState);

end;



initialization
  RegisterDrawer(TCDDrawerOvoNote.Create, dsExtra2);
end.


