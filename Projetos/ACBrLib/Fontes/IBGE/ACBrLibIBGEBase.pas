{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrLibIBGEBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibIBGEDataModule, ACBrUtil.FilesIO;

type

  { TACBrLibIBGE }

  TACBrLibIBGE = class(TACBrLib)
  private
    FIBGEDM: TLibIBGEDM;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property IBGEDM: TLibIBGEDM read FIBGEDM;

    function BuscarPorCodigo(const ACodMun: Integer; const sResposta: PChar; var esTamanho: longint):longint;
    function BuscarPorNome(const eCidade, eUF: PChar; Exata: Boolean; const sResposta: PChar;
                           var esTamanho: longint):longint;
  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig,
  ACBrLibIBGEConfig, ACBrLibIBGERespostas, ACBrObjectSerializer;

{ TACBrLibIBGE }

constructor TACBrLibIBGE.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FIBGEDM := TLibIBGEDM.Create(nil);
  FIBGEDM.Lib := Self;
end;

destructor TACBrLibIBGE.Destroy;
begin
  FIBGEDM.Free;

  inherited Destroy;
end;

procedure TACBrLibIBGE.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibIBGEConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibIBGE.Executar;
begin
  inherited Executar;
  FIBGEDM.AplicarConfiguracoes;
end;

function TACBrLibIBGE.BuscarPorCodigo(const ACodMun: Integer; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TLibIBGEResposta;
  AResposta: String;
begin
  try

    if Config.Log.Nivel > logNormal then
      GravarLog('IBGE_BuscarPorCodigo( ' + IntToStr(ACodMun) + ' )', logCompleto)
    else
      GravarLog('IBGE_BuscarPorCodigo', logNormal);

    IBGEDM.Travar;
    try
      IBGEDM.ACBrIBGE1.BuscarPorCodigo(ACodMun);
      Resp := TLibIBGEResposta.Create(1, Config.TipoResposta, Config.CodResposta);

      if (IBGEDM.ACBrIBGE1.Cidades.Count > 0) then
      begin
        Resp.Processar(IBGEDM.ACBrIBGE1.Cidades[0]);
        AResposta := Resp.Gerar;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(IBGEDM.ACBrIBGE1.Cidades.Count, AResposta);
    finally
      IBGEDM.Destravar;
      Resp.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibIBGE.BuscarPorNome(const eCidade, eUF: PChar; Exata: Boolean; const sResposta: PChar; var esTamanho: longint):longint;
var
  ACidade, AUF: AnsiString;
  Items: TArray<TLibIBGEResposta>;
  Item: TLibIBGEResposta;
  AResposta: String;
  I: Integer;
begin
  try
    ACidade := ConverterAnsiParaUTF8(eCidade);
    AUF := ConverterAnsiParaUTF8(eUF);

    if Config.Log.Nivel > logNormal then
      GravarLog('IBGE_BuscarPorNome ( ' + ACidade + ',' + AUF + ',' + BoolToStr(Exata, True) + ' )', logCompleto)
    else
      GravarLog('IBGE_BuscarPorNome', logNormal);

    IBGEDM.Travar;

    try
       IBGEDM.ACBrIBGE1.BuscarPorNome(ACidade, AUF, Exata);
       AResposta:='';

       SetLength(Items, IBGEDM.ACBrIBGE1.Cidades.Count);

       for I:= 0 to IBGEDM.ACBrIBGE1.Cidades.Count - 1 do
       begin
         Item := TLibIBGEResposta.Create(I+1, Config.TipoResposta, Config.CodResposta);
         Item.Processar(IBGEDM.ACBrIBGE1.Cidades[i]);
         Items[I] := Item;
       end;

       AResposta := TACBrObjectSerializer.Gerar<TLibIBGEResposta>(Items, Config.TipoResposta, Config.CodResposta);
       MoverStringParaPChar(AResposta, sResposta, esTamanho);
       Result := SetRetorno(IBGEDM.ACBrIBGE1.Cidades.Count, AResposta);

  finally
    IBGEDM.Destravar;
    for I := 0 to Length(Items) - 1 do
        Items[I].Free;

    Items := nil;
  end;
    except
      on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
    end;
end;

end.

