{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibCEPBase;

interface

uses
  Classes, SysUtils,
  ACBrLibComum, ACBrLibCEPDataModule, ACBrCEP, ACBrUtil.FilesIO;

type

  { TACBrLibCEP }

  TACBrLibCEP = class(TACBrLib)
  private
    FCEPDM: TLibCEPDM;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CEPDM: TLibCEPDM read FCEPDM;

    function BuscarPorCEP(eCEP: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF, eBairro: PChar;
                                 const sResposta: PChar; var esTamanho: longint): longint;
  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig,
  ACBrLibCEPConfig, ACBrLibCEPRespostas;

{ TACBrLibCEP }

constructor TACBrLibCEP.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FCEPDM := TLibCEPDM.Create(nil);
  FCEPDM.Lib := Self;
end;

destructor TACBrLibCEP.Destroy;
begin
  FCEPDM.Free;

  inherited Destroy;
end;

procedure TACBrLibCEP.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCEPConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCEP.Executar;
begin
  inherited Executar;
  FCEPDM.AplicarConfiguracoes;
end;

function TACBrLibCEP.BuscarPorCEP(eCEP: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  ACEP: AnsiString;
  Resp: TCepResposta;
  AResposta: String;
begin
  try
    ACEP := ConverterAnsiParaUTF8(eCEP);

    if Config.Log.Nivel > logNormal then
      GravarLog('CEP_BuscarPorCEP( ' + ACEP + ' )', logCompleto, True)
    else
      GravarLog('CEP_BuscarPorCEP', logNormal);


    CEPDM.Travar;
    try
      Resp := TCepResposta.Create(Config.TipoResposta, Config.CodResposta);
      CEPDM.ACBrCEP1.BuscarPorCEP(ACEP);
      Resp.Processar(CEPDM.ACBrCEP1);

      AResposta := Resp.Gerar;
      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      CEPDM.Destravar;
      Resp.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCEP.BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF, eBairro: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
var
  ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro: AnsiString;
  Resp: TCepResposta;
  AResposta: String;
begin
  try
    ACidade := ConverterAnsiParaUTF8(eCidade);
    ATipo_Logradouro := ConverterAnsiParaUTF8(eTipo_Logradouro);
    ALogradouro := ConverterAnsiParaUTF8(eLogradouro);
    AUF := ConverterAnsiParaUTF8(eUF);
    ABairro := ConverterAnsiParaUTF8(eBairro);

    if Config.Log.Nivel > logNormal then
      GravarLog('CEP_BuscarPorLogradouro( ' + ACidade + ',' + ATipo_Logradouro + ',' +
        ALogradouro + ',' + AUF + ',' +ABairro + ' )', logCompleto, True)
    else
      GravarLog('CEP_BuscarPorLogradouro', logNormal);

    CEPDM.Travar;
    try
      Resp := TCepResposta.Create(Config.TipoResposta, Config.CodResposta);

      CEPDM.ACBrCEP1.BuscarPorLogradouro(ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro);
      Resp.Processar(CEPDM.ACBrCEP1);

      AResposta := Resp.Gerar;
      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      CEPDM.Destravar;
      Resp.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

