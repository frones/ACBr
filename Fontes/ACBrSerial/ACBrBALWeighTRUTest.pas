unit ACBrBALWeighTRUTest;

{$I ACBr.inc}

interface

uses
  Classes,
  ACBrBALClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type
  { ACBrBALMarte }

  TACBrBALWeighTRUTest = class(TACBrBALClass)
  public
    constructor Create(AOwner: TComponent);

    procedure SolicitarPeso; override;
    function  InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
    procedure LeSerial(MillisecTimeOut: Integer = 200); override;
    function  LePeso(MillisecTimeOut: Integer = 3000): Double; override;
  end;

implementation


Uses
   {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Math, StrUtils, DateUtils, ACBrConsts;

{ TACBrBALWeighTRUTest }

constructor TACBrBALWeighTRUTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpModeloStr := 'Weigh TRU-Test';
end;


function TACBrBALWeighTRUTest.LePeso(MillisecTimeOut: Integer): Double;
begin
  Result := AguardarRespostaPeso(MillisecTimeOut, True);
end;


function TACBrBALWeighTRUTest.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
begin
  Result := 0;

  if aResposta = EmptyStr Then
  exit;

  wResposta := StringReplace(aResposta, '[', '', [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ']', '', [rfReplaceAll]);

  DecimalSeparator := ',';

  wResposta := StringReplace(aResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  if not TryStrToFloat(wResposta, Result) then
  Result := 0;

end;


procedure TACBrBALWeighTRUTest.LeSerial(MillisecTimeOut: Integer);
Var
  sResposta: AnsiString;
  IByte : Byte;

begin

  MillisecTimeOut  := 200;
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  Try

    //O indicador responde com: [102.5] (estável) ou [U102.5] (não estável).

    IByte := fpDevice.BytesParaLer;

    fpUltimaResposta := fpDevice.LeString(MillisecTimeOut, IByte );

    sResposta        := fpUltimaResposta;

    sResposta := StringReplace(sResposta, '[', '', [rfReplaceAll]);
    sResposta := StringReplace(sResposta, ']', '', [rfReplaceAll]);

    GravarLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

    if sResposta = EmptyStr then
    begin
      fpUltimoPesoLido := -9;
      Exit;
    end;

    if sResposta[1] <> 'U' Then
      fpUltimoPesoLido := InterpretarRepostaPeso(sResposta)
    else
      fpUltimoPesoLido := -1 // Instavel;

  except
    GravarLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) + ' , Resposta: ' + sResposta);
    fpUltimoPesoLido := -9;
  end;


end;
procedure TACBrBALWeighTRUTest.SolicitarPeso;
begin
//  inherited;
  fpDevice.Limpar;
  fpDevice.EnviaString('{RW}');
end;

end.
