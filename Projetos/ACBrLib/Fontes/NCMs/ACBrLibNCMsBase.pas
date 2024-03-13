{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrLibNCMsBase;

interface

uses
  Classes, SysUtils,
  ACBrLibComum, ACBrLibNCMsDataModule, ACBrNCMs, ACBrUtil.FilesIO;

type

  { TACBrLibNCMs }

  TACBrLibNCMs = class(TACBrLib)
  private
    FNCMsDM: TLibNCMsDM;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NCMsDM: TLibNCMsDM read FNCMsDM;

    function DescricaoNCM(const cNCM: PChar;
      const sResposta: PChar; var esTamanho: longint): longint;
    function Validar(const cNCM: PChar;
      const sResposta: PChar; var esTamanho: longint): longint;
    function BaixarLista(const cNomeArquivo: PChar;
      const sResposta: PChar; var esTamanho: longint): longint;
    function ObterNCMs(
      const sResposta: PChar; var esTamanho: longint): longint;
    function BuscarPorCodigo(const cNCM: PChar;
      const sResposta: PChar; var esTamanho: longint): longint;
    function BuscarPorDescricao(const cDesc: PChar; const nTipo: longint;
      const sResposta: PChar; var esTamanho: longint): longint;

  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig,
  ACBrLibNCMsConfig, ACBrLibNCMsRespostas,
  ACBrUtil.Base, ACBrUtil.Strings;

{ TACBrLibNCMs }

constructor TACBrLibNCMs.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FNCMsDM := TLibNCMsDM.Create(nil);
  FNCMsDM.Lib := Self;
end;

destructor TACBrLibNCMs.Destroy;
begin
  FNCMsDM.Free;

  inherited Destroy;
end;


procedure TACBrLibNCMs.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNCMsConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNCMs.Executar;
begin
  inherited Executar;
  FNCMsDM.AplicarConfiguracoes;
end;

function TACBrLibNCMs.DescricaoNCM(const cNCM: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  aNCM: String;
  AResposta: String;
begin
  try
    aNCM := ConverterAnsiParaUTF8(cNCM);
    if Config.Log.Nivel > logNormal then
      GravarLog('NCM_DescricaoNCM( ' + cNCM + ' )', logCompleto, True)
    else
      GravarLog('NCM_DescricaoNCM', logNormal);

    NCMsDM.Travar;
    try
      AResposta := NCMsDM.ACBrNCMs1.DescricaoNcm(aNCM);

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNCMs.Validar(const cNCM: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
var
  aNCM: String;
  NCMehValido: Boolean;
  AResposta: String;
begin
  try
    aNCM := ConverterAnsiParaUTF8(cNCM);
    if Config.Log.Nivel > logNormal then
      GravarLog('NCM_Validar( ' + cNCM + ' )', logCompleto, True)
    else
      GravarLog('NCM_Validar', logNormal);

    NCMsDM.Travar;
    try
      aNCM := OnlyNumber(aNCM);

      if (Length(aNCM) <> 8) then
        raise Exception.Create('O codigo do NCM deve conter 8 Caracteres');

      NCMehValido := NCMsDM.ACBrNCMs1.Validar(aNCM);
      if NCMehValido  then
         AResposta := 'NCM Valido'
      else
         raise Exception.Create('NCM Invalido');

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNCMs.BaixarLista(const cNomeArquivo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  aNomeArquivo: String;
  AResposta: String;
begin
  try
    aNomeArquivo := ConverterAnsiParaUTF8(cNomeArquivo);
    if Config.Log.Nivel > logNormal then
      GravarLog('NCM_BaixarLista( ' + aNomeArquivo + ' )', logCompleto, True)
    else
      GravarLog('NCM_BaixarLista', logNormal);

    NCMsDM.Travar;
    try
      NCMsDM.ACBrNCMs1.ObterNCMs;
      NCMsDM.ACBrNCMs1.NCMS.SaveToFile(aNomeArquivo);

      AResposta := 'Arquivo salvo em: ' + aNomeArquivo;
      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNCMs.ObterNCMs(const sResposta: PChar; var esTamanho: longint): longint;
var
  AResposta: String;
begin
  try
    GravarLog('NCM_ObterNCMs', logNormal);

    NCMsDM.Travar;
    try
      NCMsDM.ACBrNCMs1.ObterNCMs;

      AResposta := 'Lista de NCMs atualizada';
      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNCMs.BuscarPorCodigo(const cNCM: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  AResposta: String;
  NCMsResposta: TNCMsRespostaFactory;
  wFiltro: String;
begin
  try
    wFiltro := ConverterAnsiParaUTF8(cNCM);
    if Config.Log.Nivel > logNormal then
      GravarLog('NCM_BuscarPorCodigo( ' + wFiltro + ' )', logCompleto, True)
    else
      GravarLog('NCM_BuscarPorCodigo', logNormal);

    NCMsDM.Travar;
    try
      wFiltro:= Trim(wFiltro);

      if EstaVazio(wFiltro) then
        raise Exception.Create('Filtro nao informado');

      NCMsDM.ACBrNCMs1.BuscarPorCodigo(wFiltro);

      if (NCMsDM.ACBrNCMs1.NCMsFiltrados.Count <= 0) then
        raise Exception.Create('Nenhum NCM encontrado com codigo: ' + wFiltro);

      NCMsResposta := TNCMsRespostaFactory.Create(False, Config.TipoResposta, Config.CodResposta);
      try
        NCMsResposta.Processar(NCMsDM.ACBrNCMs1);
        AResposta := NCMsResposta.Gerar;
      finally
        NCMsResposta.Free;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNCMs.BuscarPorDescricao(const cDesc: PChar; const nTipo: longint; const sResposta: PChar;
  var esTamanho: longint): longint;
var
  AResposta: String;
  wTipoFiltro: Integer;
  wFiltro, wMsgErro: String;
  NCMsResposta: TNCMsRespostaFactory;
begin
  try
    wFiltro := ConverterAnsiParaUTF8(cDesc);
    wTipoFiltro := nTipo;

    if Config.Log.Nivel > logNormal then
      GravarLog('NCM_BuscarPorDescricao( ' + wFiltro + ',' + IntToStr(nTipo) + ' )', logCompleto, True)
    else
      GravarLog('NCM_BuscarPorDescricao', logNormal);

    NCMsDM.Travar;
    try
      wFiltro:= Trim(wFiltro);

      if EstaVazio(wFiltro) then
        raise Exception.Create('Filtro nao informado');

      NCMsDM.ACBrNCMs1.BuscarPorDescricao(wFiltro, TACBrNCMTipoFiltro(wTipoFiltro));

      if (NCMsDM.ACBrNCMs1.NCMsFiltrados.Count <= 0) then
      begin
        case wTipoFiltro of
          1: wMsgErro := 'contendo';
          2: wMsgErro := 'finalizada com';
        else
          wMsgErro := 'iniciando com';
        end;

        raise Exception.Create(Format('Nenhum NCM encontrado com descricao %s a string %s',
          [wMsgErro, QuotedStr(wFiltro)]));
      end;

      NCMsResposta := TNCMsRespostaFactory.Create(False, Config.TipoResposta, Config.CodResposta);
      try
        NCMsResposta.Processar(NCMsDM.ACBrNCMs1);
        AResposta := NCMsResposta.Gerar;
      finally
        NCMsResposta.Free;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      NCMsDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;


end.

