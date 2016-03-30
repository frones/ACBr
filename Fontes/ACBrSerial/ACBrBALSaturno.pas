unit ACBrBALSaturno;

interface
uses ACBrBALClass,
     Classes;

type
 TACBrBALSaturno = class( TACBrBALClass )
  public
    constructor Create(AOwner: TComponent);
    function LePeso( MillisecTimeOut : Integer = 3000) :Double; override;
    procedure LeSerial( MillisecTimeOut : Integer = 500) ; override ;
  end ;

implementation
Uses ACBrUtil, ACBrConsts,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} synaser, Windows{$ENDIF},
     SysUtils, Math, ACBrDevice,dialogs ;

{ TACBrBALSaturno }

constructor TACBrBALSaturno.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Saturno' ;
end;

function TACBrBALSaturno.LePeso( MillisecTimeOut : Integer) : Double;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;

  GravaLog('- '+FormatDateTime('hh:nn:ss:zzz',now)+' TX -> '+#05 );
  fpDevice.Limpar;                  { Limpa a Porta }
  fpDevice.EnviaString( #05 );      { Envia comando solicitando o Peso }
  sleep(200) ;

  LeSerial( MillisecTimeOut );

  Result := fpUltimoPesoLido ;
end;

procedure TACBrBALSaturno.LeSerial(MillisecTimeOut: Integer);
Var
  Resposta : AnsiString ;
  bAchouE_O: Boolean;
  posicaoE_O, Decimais : Integer ;
begin
  fpUltimoPesoLido := 0 ;
  fpUltimaResposta := '' ;
  Decimais := 1000 ;
  Try
    fpUltimaResposta := trim(fpDevice.Serial.RecvPacket( MillisecTimeOut));
    bAchouE_O := False;
    posicaoE_O := 0;

    // Se encontrar a letra 'E' (Estável) ou 'O' (Oscilante), captura o peso da
    // posição 1 a 7 da string
    if (Pos('E',UpperCase(fpUltimaResposta)) > 0) or (Pos('O',UpperCase(fpUltimaResposta)) > 0) then
    begin
      if Pos('E',UpperCase(fpUltimaResposta)) > 0 then
        posicaoE_O := Pos('E',UpperCase(fpUltimaResposta))
      else
        posicaoE_O := Pos('O',UpperCase(fpUltimaResposta));

      bAchouE_O := True;

      Resposta := Copy(fpUltimaResposta,0 , posicaoE_O - 1);
    end;

    // Removendo caracteres especiais, caso encontre algum
    if bAchouE_O then
    begin
      Resposta := StringReplace(Resposta, '°', '0', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '±', '1', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '²', '2', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '³', '3', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '´', '4', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, 'µ', '5', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '¶', '6', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '·', '7', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '¸', '8', [rfReplaceAll]);
      Resposta := StringReplace(Resposta, '¹', '9', [rfReplaceAll]);
    end;

    if Length(Resposta) > 0 then
    begin
      try
        fpUltimoPesoLido := StrToFloat(Resposta);
      except
        case Resposta[1] of
          'I' : fpUltimoPesoLido := -1  ;  { Instavel }
          'N' : fpUltimoPesoLido := -2  ;  { Peso Negativo }
          'S' : fpUltimoPesoLido := -10 ;  { Sobrecarga de Peso }
        else
          fpUltimoPesoLido := 0 ;
        end;
      end;
    end
    else
      fpUltimoPesoLido := 0 ;
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9 ;
  end ;
end;

end.

