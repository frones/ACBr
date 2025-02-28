{$I ACBr.inc}

unit ACBrBALTrentin;

interface

uses
  ACBrBALClass,
  Classes;

const STX = #02;
      CR  = #13;

type
  TACBrBALTrentin = class( TACBrBALClass )
  private
    fpProtocolo: AnsiString;
    fpDecimais: Integer;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; override;
    function ProtocoloCondecDetectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
    function InterpretarProtocoloEL05(const aResposta: AnsiString): AnsiString;
    function InterpretarProtocoloCondec(const aResposta: AnsiString): AnsiString;
    function PadLeft(pString: String; Tamanho: Integer): string;
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation

uses
  ACBrBAL,
  ACBrUtil.Strings,
  ACBrConsts,
  DateUtils,
  StrUtils,
  Math,
  SysUtils;

{ TACBrBALTrentin }

constructor TACBrBALTrentin.Create(AOwner: TComponent);
begin
    inherited Create( AOwner );

    fpModeloStr := 'Trentin' ;
    fpProtocolo := 'Não Definido';
    fpDecimais  := 1000;
end;

function TACBrBALTrentin.LePeso( MillisecTimeOut : Integer) : Double;
begin
    fpDevice.Serial.Purge ;           { Limpa a Porta }
    fpDevice.EnviaString(#05);        { Envia comando solicitando o Peso }
    sleep(200) ;

    LeSerial(MillisecTimeOut);

    Result := fpUltimoPesoLido;
end;

procedure TACBrBALTrentin.LeSerial(MillisecTimeOut: Integer);
begin
    fpUltimoPesoLido := 0;
    fpUltimaResposta := '';

    try
        fpUltimaResposta := fpDevice.Serial.RecvPacket(MillisecTimeOut);
        fpUltimoPesoLido := InterpretarRepostaPeso(fpUltimaResposta);
    except
        // Peso não foi recebido (TimeOut)
        fpUltimoPesoLido := -9;
    end;
end;

function TACBrBALTrentin.ProtocoloCondecDetectado(const wPosIni: Integer; const aResposta: AnsiString): Boolean;
begin
    if  (aResposta = EmptyStr) then begin
        Result := False;
        Exit;
    end;
    Result := Pos(STX, aResposta) > 0;
end;

function TACBrBALTrentin.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
    wPosIni: Integer;
    wResposta: AnsiString;
begin
    Result  := 0;

    wPosIni := PosLast(STX, aResposta);

    if  ProtocoloCondecDetectado(wPosIni, aResposta) then
        wResposta := InterpretarProtocoloCondec(aResposta)
    else
       //protocolo el05
       wResposta := InterpretarProtocoloel05(aResposta);

    if  (wResposta = EmptyStr) then
        Exit;

    { Ajustando o separador de Decimal corretamente }
    wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
    wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

    try
        { Já existe ponto decimal ? }
        if  Pos(DecimalSeparator, wResposta) > 0 then
            Result := StrToFloat(wResposta)
        else
            Result := (StrToInt(wResposta) / fpDecimais);
    except
        // O = overflow / M = movimento / espaço (20h) = peso válido
        case PadLeft(Trim(wResposta),3)[1] of
          'O': Result := -10;  { Sobrecarga de Peso }
          'M': Result := -11;  { Indica peso em captura inicial de zero (dispositivo não está pronta para pesar) }
        else
          Result := 0;
        end;
    end;
end;

function TACBrBALTrentin.InterpretarProtocoloEL05(const aResposta: AnsiString): AnsiString;
var
    s : string;
    p: Integer;
begin
    {Protocolo EL05:
      [status] = 1 byte
      [ PESO ] = 6 caracteres sem ponto decimal, com zeros a esquerda
      [ CR   ] }

    { Localiza a posiçção do enter na string, a partir do enter copia 6 posicoes para tras
      EX: D000658CRD000658CR
          12345678 901234567

     A atring veio quebrada, nao conseguiu copiar os 6 caracteres, nao retorna o peso
      EX: 00658CRD000658CR  }

    p := Pos(CR, aResposta);
    if  p-6 < 1 then begin
        Result := '0';
        Exit;
    end;

    s := Copy(aResposta, p-6, 6);
    Result := Trim(OnlyNumber(s));
end;

function TACBrBALTrentin.InterpretarProtocoloCondec(const aResposta: AnsiString): AnsiString;
var
    l_strpso: string;
    l_posini, l_posfim: Integer;
begin
    fpProtocolo := 'Protocolo Condec';
    // localiza o primeiro STX, e depois procura pelo CR para obter a string do peso
    l_posini := Pos(STX, aResposta);
    l_posfim := PosEX(CR, aResposta, l_posini + 1);
    if  l_posfim = 0 then
        l_posfim := Length(aResposta);

    // <STX><pol><peso><K/L><G/N><status><CR><LF>
    //   ^                                 ^
    // Separa a primeira string contendo o peso
    l_strpso := Copy(aResposta, l_posini, l_posfim - l_posini + 1);

    // obtem o peso da string lida
    Result := Copy(l_strpso, 3, 7);
end;

function TACBrBALTrentin.PadLeft(pString: String; Tamanho: Integer): string;
begin
    Result := Format('%*s',[Tamanho, pString]);
end;

end.
