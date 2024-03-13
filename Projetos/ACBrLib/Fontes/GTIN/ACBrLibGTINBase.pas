{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }

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

unit ACBrLibGTINBase;

interface

uses
  Classes, SysUtils,
  ACBrLibComum, ACBrLibGTINDataModule, ACBrGTIN, ACBrUtil.FilesIO;

type

   { TACBrLibGTIN }

  TACBrLibGTIN = class (TACBrLib)
  private
    FGTINDM: TLibGTINDM;

  protected
    procedure CriarConfiguracao (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property GTINDM: TLibGTINDM read FGTINDM;

    function Consultar (aGTIN: PChar; const sResposta: PChar; var esTamanho: longint): longint;

  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig,
  ACBrLibGTINConfig, ACBrLibGTINRespostas;

{ TACBrLibGTIN }

constructor TACBrLibGTIN.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FGTINDM := TLibGTINDM.Create(Nil);
  FGTINDM.Lib := Self;
end;

destructor TACBrLibGTIN.Destroy;
begin
  FGTINDM.Free;

  inherited Destroy;
end;

procedure TACBrLibGTIN.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibGTINConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibGTIN.Executar;
begin
  inherited Executar;
  FGTINDM.AplicarConfiguracoes;
end;

function TACBrLibGTIN.Consultar(aGTIN: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  GTIN, AResposta: String;
  Resp: TGTINResposta;
begin
  try
    GTIN:= AnsiString(aGTIN);

    if Config.Log.Nivel > logNormal then
     GravarLog('GTIN_Consultar (' + GTIN + ' ) ', logCompleto, True)
    else
     GravarLog('GTIN_Consultar', logNormal);

    GTINDM.Travar;
    try
      GTINDM.ACBrGTIN1.Consultar(GTIN);
      AResposta:= '';

      Resp:= TGTINResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(GTINDM.ACBrGTIN1);
        AResposta:= Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, StrPas(sResposta));

    finally
      GTINDM.Destravar;
    end;

  except
    on E: EACBrLibException do
     Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
     Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

