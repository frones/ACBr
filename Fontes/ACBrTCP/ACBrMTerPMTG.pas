{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2016 Elias César Vieira                     }
{                                                                              }
{ Colaboradores nesse arquivo: Daniel Simões de Almeida                        }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 17/05/2016: Elias César Vieira
|*  - Primeira Versao ACBrMTerPMTG
******************************************************************************}

{$I ACBr.inc}

unit ACBrMTerPMTG;

interface

uses
  Classes, SysUtils, ACBrMTerClass, ACBrConsts, ACBrUtil;

type

  { TACBrMTerPMTG }

  TACBrMTerPMTG = class(TACBrMTerClass)
  private
    function PrepararCmd(aCmd: Integer; const aParams: AnsiString = ''): AnsiString; overload;
    function PrepararCmd(aCmd: Integer; const aParams: Integer): AnsiString; overload;
    function PrepararCmd(aCmd: Integer; const aParams: Boolean): AnsiString; overload;

  public
    constructor Create(aOwner: TComponent);

    procedure ComandoBackSpace(Comandos: TACBrMTerComandos); override;
    procedure ComandoBoasVindas(Comandos: TACBrMTerComandos); override;
    procedure ComandoBeep(Comandos: TACBrMTerComandos; const aTempo: Integer = 0);
      override;
    procedure ComandoDeslocarCursor(Comandos: TACBrMTerComandos; const aValue: Integer);
      override;
    procedure ComandoDeslocarLinha(Comandos: TACBrMTerComandos; aValue: Integer);
      override;
    procedure ComandoEco(Comandos: TACBrMTerComandos; const aValue: AnsiString); override;
    procedure ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
      const aDados: AnsiString); override;
    procedure ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
      const aDados: AnsiString; aSerial: Byte = 0); override;
    procedure ComandoEnviarTexto(Comandos: TACBrMTerComandos; const aTexto: AnsiString);
      override;
    procedure ComandoLimparDisplay(Comandos: TACBrMTerComandos); override;
    procedure ComandoLimparLinha(Comandos: TACBrMTerComandos; const aLinha: Integer);
      override;
    procedure ComandoOnline(Comandos: TACBrMTerComandos); override;
    procedure ComandoPosicionarCursor(Comandos: TACBrMTerComandos; const aLinha,
      aColuna: Integer); override;

    function ExtrairResposta(var ABuffer: Ansistring; LendoPeso: Boolean = False
      ): AnsiString; override;
    function InterpretarResposta(const aRecebido: AnsiString): AnsiString; override;
  end;

implementation

uses
  math;

{ TACBrMTerPMTG }

function TACBrMTerPMTG.PrepararCmd(aCmd: Integer; const aParams: AnsiString): AnsiString;
var
  wTamParams: Integer;
begin
  wTamParams := Length(aParams);
  Result     := IntToLEStr(aCmd) + IntToLEStr(wTamParams) + aParams;
end;

function TACBrMTerPMTG.PrepararCmd(aCmd: Integer; const aParams: Integer
  ): AnsiString;
begin
  Result := PrepararCmd( aCmd, IntToLEStr(aParams, 4));
end;

function TACBrMTerPMTG.PrepararCmd(aCmd: Integer; const aParams: Boolean
  ): AnsiString;
begin
   Result := PrepararCmd(aCmd, ifthen(aParams, 1, 0));
end;

constructor TACBrMTerPMTG.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  fpModeloStr := 'PMTG';
end;

procedure TACBrMTerPMTG.ComandoBackSpace(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd(33), TimeOut );     // 0021h - vBackSpace
end;

procedure TACBrMTerPMTG.ComandoBoasVindas(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd(3), TimeOut );      // 0003h - wGetIdentify
end;

procedure TACBrMTerPMTG.ComandoBeep(Comandos: TACBrMTerComandos;
  const aTempo: Integer);
begin
  Comandos.New( PrepararCmd(93, True), aTempo*-1);   //  005Dh - vSetBeep - On
  Comandos.New( PrepararCmd(93, False), TimeOut);    //  005Dh - vSetBeep - Off
end;

procedure TACBrMTerPMTG.ComandoDeslocarCursor(Comandos: TACBrMTerComandos;
  const aValue: Integer);
begin
  Comandos.New( PrepararCmd(43, NUL + AnsiChr(aValue)), TimeOut );   // 002Bh - vGoToXYRef  - Byte 1, Linha; Byte 2 Coluna
end;

procedure TACBrMTerPMTG.ComandoDeslocarLinha(Comandos: TACBrMTerComandos;
  aValue: Integer);
begin
  Comandos.New( PrepararCmd(43, AnsiChr(aValue) + NUL), TimeOut );   // 002Bh - vGoToXYRef  - Byte 1, Linha; Byte 2 Coluna
end;

procedure TACBrMTerPMTG.ComandoEco(Comandos: TACBrMTerComandos;
  const aValue: AnsiString);
var
  P: Integer;
  S: AnsiString;
begin
  S := LimparConteudoParaEnviar(aValue);   // DEBUG S := 'DANIEL'+BS+'SIMOES'+BS+'DE'+BS+BS+'ALMEIDA';
  while (S <> EmptyStr) do
  begin
    P := pos(BS, S+BS);
    ComandoEnviarTexto(Comandos, copy(S,1,P-1));
    if P <= Length(S) then
      ComandoBackSpace(Comandos);

    S := copy(S, P+1, Length(S));
  end;
end;

procedure TACBrMTerPMTG.ComandoEnviarParaParalela(Comandos: TACBrMTerComandos;
  const aDados: AnsiString);
var
  P, LenBloco: Integer;
  wBloco, ArgStrByte: String;
begin
  { Apenas GE750 possui porta Paralela }
  P := 1;
  while (P <= Length(aDados)) do
  begin
    wBloco := copy(aDados, P, 256);
    LenBloco := Length(wBloco);
    wBloco := wBloco + StringOfChar( NUL, 256-LenBloco );
    ArgStrByte := wBloco + chr(LenBloco);
    Comandos.New( PrepararCmd(73, ArgStrByte), TimeOut );      // 0049h - cSendPrn
    Inc(P, 256);
  end;
end;

procedure TACBrMTerPMTG.ComandoEnviarParaSerial(Comandos: TACBrMTerComandos;
  const aDados: AnsiString; aSerial: Byte);
var
  ArgComBinByte, wBloco: AnsiString;
  P, LenBloco: Integer;
begin
  { Portas COM disponíveis:
    - GE750: 1 e 2;
    - GE760: 1;
    - MT740: 1, 2, 3 e 4;
    - MT720: 1, 2 e 3 }

  Comandos.New( PrepararCmd(57, AnsiChr(aSerial-1) + IntToLEStr(1,4) ), TimeOut );  // 0039h - vSetEnableSerial - On

  P := 1;
  while (P <= Length(aDados)) do
  begin
    wBloco := copy(aDados, P, 256);
    LenBloco := Length(wBloco);
    wBloco := wBloco + StringOfChar( NUL, 256-LenBloco );
    ArgComBinByte := AnsiChr(aSerial-1) + wBloco + chr(LenBloco);
    Comandos.New( PrepararCmd(63, ArgComBinByte), TimeOut );             // 003Fh - SendBinSerial
    Inc(P, 256);
  end;
end;

procedure TACBrMTerPMTG.ComandoEnviarTexto(Comandos: TACBrMTerComandos;
  const aTexto: AnsiString);
begin
  Comandos.New( PrepararCmd(51, aTexto + NUL), TimeOut );
end;

procedure TACBrMTerPMTG.ComandoLimparDisplay(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd(39), TimeOut );    // 0027h - vFormFeed
end;

procedure TACBrMTerPMTG.ComandoLimparLinha(Comandos: TACBrMTerComandos;
  const aLinha: Integer);
begin
  Comandos.New( PrepararCmd(55, AnsiChr(aLinha)), TimeOut );   // 0037h - vDispClrLn
end;

procedure TACBrMTerPMTG.ComandoOnline(Comandos: TACBrMTerComandos);
begin
  Comandos.New( PrepararCmd(1), TimeOut );     // 0001h - vLive
end;

procedure TACBrMTerPMTG.ComandoPosicionarCursor(Comandos: TACBrMTerComandos;
  const aLinha, aColuna: Integer);
begin
  // 0029h - vGoToXY - Byte 1, Linha; Byte 2 Coluna
  Comandos.New( PrepararCmd(41, AnsiChr(aLinha) + AnsiChr(aColuna) ), TimeOut );
end;

function TACBrMTerPMTG.ExtrairResposta(var ABuffer: Ansistring;
  LendoPeso: Boolean): AnsiString;
var
  wLen, wLenParams: Integer;
begin
  Result := '';
  wLen := Length(ABuffer);
  if wLen < 4 then
    Exit;

  wLenParams := LEStrToInt(Copy(ABuffer, 3, 2));
  if wLen < (wLenParams+4) then
    Exit;

  Result := copy(ABuffer, 1, wLenParams+4);
  ABuffer := copy(ABuffer, wLenParams+5, Length(ABuffer) );
end;

function TACBrMTerPMTG.InterpretarResposta(const aRecebido: AnsiString): AnsiString;
var
  wCmd, wLenParams, wLen, wPosCmd, wLenBuffer: Integer;
  wParam: AnsiString;
begin
  Result  := '';
  wLen    := Length(aRecebido);
  wPosCmd := 1;

  while (wPosCmd < wLen) do
  begin
    wCmd       := LEStrToInt(Copy(aRecebido, wPosCmd, 2));
    wLenParams := LEStrToInt(Copy(aRecebido, wPosCmd + 2, 2));
    wParam     := Copy(aRecebido, wPosCmd + 4, wLenParams);

    if (Length(wParam) > 0) then
    begin
      case wCmd of
        29:   // 001Dh - vGetCharTerm
          Result := Result + wParam;

        61:   // 003Dh - vGetBinSerial
        begin
          //    1 Byte , Porta
          //  256 Bytes, Buffer
          //    1 Byte , Tamanho Buffer
          wLenBuffer := ord(wParam[Length(wParam)]);
          Result := Result + Copy(wParam, 2, wLenBuffer);
        end;
      end;
    end

    else if (wCmd = 2) then   // 0002h - vLive
    begin
      Result := Result + 'OnLine';
    end;

    wPosCmd := wPosCmd + wLenParams + 4;
  end;
end;

end.

