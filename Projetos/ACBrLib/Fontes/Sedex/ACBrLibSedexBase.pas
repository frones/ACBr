{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibSedexBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibComum, ACBrLibSedexDataModule, ACBrSedex;

type

  { TACBrLibSedex }

  TACBrLibSedex = class(TACBrLib)
  private
    FSedexDM: TLibSedexDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property SedexDM: TLibSedexDM read FSedexDM;

    function Consultar(const eArqIni: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
    function Rastrear(const eCodRastreio: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibSedexConfig,
  ACBrLibResposta, ACBrLibSedexRespostas;

{ TACBrLibSedex }

constructor TACBrLibSedex.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FSedexDM := TLibSedexDM.Create(nil);
  FSedexDM.Lib := Self;
end;

destructor TACBrLibSedex.Destroy;
begin
  FSedexDM.Free;
  inherited Destroy;
end;

procedure TACBrLibSedex.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibSedex.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibSedex.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibSedexConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibSedex.Executar;
begin
  inherited Executar;
  FSedexDM.AplicarConfiguracoes;
end;

function TACBrLibSedex.Consultar(const eArqIni: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TLibSedexConsulta;
  AArqIni, AResposta: String;
begin
  try
    AArqIni := ConverterStringEntrada(eArqIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('Sedex_Consultar( ' + AArqIni + ' )', logCompleto, True)
    else
      GravarLog('Sedex_Consultar', logNormal);


    SedexDM.Travar;

    try
      if not SedexDM.ACBrSedex1.LerArqIni(AArqIni) then
        raise EACBrLibException.Create(-99, 'Erro ao ler o dados para consulta.');

      SedexDM.ACBrSedex1.Consultar;
      Resp := TLibSedexConsulta.Create(Config.TipoResposta, Config.CodResposta);
      Resp.Processar(SedexDM.ACBrSedex1);
      AResposta := Resp.Gerar;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      Resp.Free;
      SedexDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibSedex.Rastrear(const eCodRastreio: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACodRastreio: AnsiString;
  Rastreios: TArray<TLibSedexRastreio>;
  Item: TLibSedexRastreio;
  Resposta: String;
  I: Integer;
begin
  try
    ACodRastreio := ConverterStringEntrada(eCodRastreio);

    if Config.Log.Nivel > logNormal then
      GravarLog('Sedex_Rastrear( ' + ACodRastreio + ' )', logCompleto, True)
    else
      GravarLog('Sedex_Rastrear', logNormal);

    SedexDM.Travar;

    try
      SedexDM.ACBrSedex1.Rastrear(ACodRastreio);
      Resposta := '';

      SetLength(Rastreios, SedexDM.ACBrSedex1.retRastreio.Count);

      for I := 0 to SedexDM.ACBrSedex1.retRastreio.Count - 1 do
      begin
        Item := TLibSedexRastreio.Create(I+1, Config.TipoResposta, Config.CodResposta);
        Item.Processar(SedexDM.ACBrSedex1.retRastreio[i]);
        Rastreios[I] := Item;
      end;

      Resposta := GerarResposta<TLibSedexRastreio>(Rastreios);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      SedexDM.Destravar;
      for I := 0 to Length(Rastreios) - 1 do
        Rastreios[I].Free;

      Rastreios := nil;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

end.

