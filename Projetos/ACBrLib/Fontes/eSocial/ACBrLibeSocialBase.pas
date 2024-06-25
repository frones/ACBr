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

unit ACBrLibeSocialBase;

interface

uses
  Classes, SysUtils, Forms, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibComum, ACBrLibeSocialDataModule, ACBreSocial, pcesConversaoeSocial, ACBrLibeSocialRespostas;

type

  { TACBrLibeSocial }

  TACBrLibeSocial = class(TACBrLib)
  private
    FeSocialDM: TLibeSocialDM;

    function SetRetornoEventoCarregados(const NumEventos: integer): integer;
    function SetRetornoeSocialCarregadas(const NumeSocial: integer): integer;

  protected
    procedure CriarConfiguracao (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    function CriarEventoeSocial (eArqIni: PChar):longint;
    function EnviareSocial (aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultareSocial (eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CriarEnviareSocial (const eArqIni: PChar; aGrupo:integer): longint;
    function LimpareSocial: Longint;
    function CarregarXMLEventoeSocial (const eArquivoOuXML: PChar): longint;
    function SetIDEmpregador (const aIdEmpregador: PChar): longint;
    function SetIDTransmissor (const aIdTransmissor: PChar): longint;
    function SetTipoEmpregador (aTipoEmpregador: integer):longint;
    function SetVersaoDF (const sVersao: PChar):longint;
    function ConsultaIdentificadoresEventosEmpregador (const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
    function ConsultaIdentificadoresEventosTabela (const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
    function ConsultaIdentificadoresEventosTrabalhador (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
    function DownloadEventos (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
    function ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
    function Validar: longint;

    property eSocialDM: TLibeSocialDM read FeSocialDM;

  end;

implementation

Uses
  ACBrLibConsts, ACBrLibeSocialConsts, ACBrLibConfig, ACBrLibeSocialConfig,
  ACBrLibResposta, ACBrLibCertUtils, StrUtils;

{ TACBrLibeSocial }

constructor TACBrLibeSocial.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited
  Create(ArqConfig, ChaveCrypt);
  FeSocialDM := TLibeSocialDM.Create(Nil);
  FeSocialDM.Lib := Self;
end;

destructor TACBrLibeSocial.Destroy;
begin
  FeSocialDM.Free;

  inherited Destroy;
end;

procedure TACBrLibeSocial.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibeSocialConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibeSocial.Executar;
begin
  inherited Executar;
  FeSocialDM.AplicarConfiguracoes;
end;

function TACBrLibeSocial.SetRetornoEventoCarregados(const NumEventos: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfEventosCarregados, [NumEventos]));
end;

function TACBrLibeSocial.SetRetornoeSocialCarregadas(const NumeSocial: integer):integer;
begin
  Result := SetRetorno(0, Format(SInfeSocialCarregadas, [NumeSocial]));
end;

function TACBrLibeSocial.CriarEventoeSocial(eArqIni: PChar): longint;
var
  AArqIni: String;
begin
  try
    AArqIni:= AnsiString(eArqIni);

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_CriarEvento (' + AArqIni + ' ) ', logCompleto, True)
    else
     GravarLog('eSocial_CriarEvento', logNormal);

    eSocialDM.Travar;
    try
      eSocialDM.ACBreSocial1.Eventos.LoadFromFile(AArqIni, False);
      Result := SetRetorno(ErrOK);

    finally
     eSocialDM.Destravar;
    end;

  except
    on E:EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.EnviareSocial(aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
var
  AResposta: string;
  Resp: TEnvioResposta;
  grupo: Integer;
begin
  grupo := aGrupo;

  try
    if Config.Log.Nivel > logNormal then
      GravarLog('eSocial_EnviareSocial (' + IntToStr(Grupo) + ' ) ', logCompleto, True)
    else
      GravarLog('eSocial_EnviareSocial', logNormal);

    eSocialDM.Travar;
    try
      eSocialDM.ACBreSocial1.Enviar(TeSocialGrupo(Grupo));
      AResposta:= '';

      Resp := TEnvioResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(eSocialDM.ACBreSocial1);
        AResposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      eSocialDM.ACBreSocial1.Eventos.Clear;

      MoverStringParaPChar (AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);

    finally
       eSocialDM.Destravar;
    end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.ConsultareSocial(eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  AProtocolo, AResposta: String;
  Resp : TConsulta;
begin
  try
    AProtocolo:= AnsiString(eProtocolo);

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_ConsultareSocial (' + AProtocolo + ' ) ', logCompleto, True)
    else
     GravarLog('eSocial_ConsultareSocial', logNormal);

     eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Eventos.Clear;
        esocialDM.ACBreSocial1.Consultar(AProtocolo);
        AResposta:= '';

        Resp := TConsulta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(eSocialDM.ACBreSocial1);
          AResposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, AResposta);

      finally
        eSocialDM.Destravar;
      end;
      
  except
    on E: EACBrLibException do
     Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
     Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.CriarEnviareSocial(const eArqIni: PChar; aGrupo:integer): longint;
var
  AIniFile, ArqeSocial : String;
  ASalvar : Boolean;
  iEvento : Integer;
  grupo : Integer;
  AResposta: String;
  Resp : TEnvioResposta;
begin
  AIniFile:= AnsiString(eArqIni);
  Grupo:= aGrupo;
  try

     if Config.Log.Nivel > logNormal then
       GravarLog('eSocial_CriarEnviareSocial(' + AIniFile + ', ' + IntToStr(Grupo) + ' )', logCompleto, True)
     else
       GravarLog('eSocial_CriarEnviareSocial', logNormal);

     if not FileExists(AIniFile) then
       raise EACBrLibException.Create(ErrArquivoNaoExiste, ACBrStr(Format(SErroeSocialAbrir, [AIniFile])));

        eSocialDM.Travar;
        try
           eSocialDM.ACBreSocial1.Eventos.LoadFromIni(AIniFile);
           ASalvar:= eSocialDM.ACBreSocial1.Configuracoes.Geral.Salvar;
           AResposta:= '';

           if not ASalvar then
            begin
              ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath);
              eSocialDM.ACBreSocial1.Configuracoes.Arquivos.PathSalvar:= PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath;
            end;

           iEvento:= eSocialDM.ACBreSocial1.Eventos.Gerados.Count - 1;
           ArqeSocial:= eSocialDM.ACBreSocial1.Eventos.Gerados.Items[iEvento].PathNome + '.xml';

           if not FileExists(ArqeSocial) then
            raise EACBrLibException.Create(ErrArquivoNaoExiste, ACBrStr(Format(SErroeSocialAbrir, [ArqeSocial]) ));

           AResposta:= ArqeSocial + sLineBreak + ACBrStr(Format(SMsgeSocialEventoAdicionado, [TipoEventoToStr(eSocialDM.ACBreSocial1.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) + sLineBreak;

           Result := SetRetorno(ErrOK, AResposta);

           eSocialDM.ACBreSocial1.Enviar(TeSocialGrupo(Grupo));
           Sleep(3000);

           Resp := TEnvioResposta.Create(Config.TipoResposta, Config.CodResposta);
           try
             Resp.Processar(eSocialDM.ACBreSocial1);
             AResposta := AResposta + sLineBreak + Resp.Gerar;
           finally
             Resp.Free;
           end;

           Result := SetRetorno(ErrOK, AResposta);

           eSocialDM.ACBreSocial1.Eventos.Clear;

        finally
          eSocialDM.Destravar;
        end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.LimpareSocial: Longint;
begin
  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_Limpar', logCompleto, True)
     else
      GravarLog('eSocial_Limpar', logNormal);

     eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Eventos.Clear;
        Result := SetRetorno(ErrOK);
      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibeSocial.CarregarXMLEventoeSocial(const eArquivoOuXML: PChar): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_CarregarXMLEventoeSocial(' + ArquivoOuXml + ' ) ', logCompleto, True)
    else
     GravarLog('eSocial_CarregarXMLEventoeSocial', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    eSocialDM.Travar;

    try
      if EhArquivo then
      eSocialDM.ACBreSocial1.Eventos.LoadFromFile(ArquivoOuXml)
      else
       eSocialDM.ACBreSocial1.Eventos.LoadFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(eSocialDM.ACBreSocial1.Eventos.Count);
    finally
      eSocialDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.SetIDEmpregador (const aIdEmpregador: PChar): longint;
var
  idEmpregador: AnsiString;
begin
  try
    idEmpregador:= ConverterAnsiParaUTF8(aIdEmpregador);

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_SetIDEmpregador (' + idEmpregador + ')', logCompleto, True)
    else
     GravarLog('eSocial_SetIDEmpregador', logNormal);

    if DirectoryExists(idEmpregador) then
      raise EACBrLibException.Create(ErrDiretorioNaoExiste, 'Diretorio não existe');

      eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Configuracoes.Geral.IdEmpregador := idEmpregador;
        Result := SetRetorno(ErrOK);
      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibeSocial.SetIDTransmissor (const aIdTransmissor: PChar): longint;
var
  idTransmissor: AnsiString;
begin
  try
    idTransmissor:= ConverterAnsiParaUTF8(aIdTransmissor);

    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_SetIDTransmissor(' + idTransmissor + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_SetIDTransmissor', logNormal);

    if EstaVazio(idTransmissor)then
         raise EACBrLibException.Create(ErrParametroInvalido, 'Valor Nulo');

      eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Configuracoes.Geral.IdTransmissor := idTransmissor;
        Result := SetRetorno(ErrOK);
      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.SetTipoEmpregador (aTipoEmpregador: integer):longint;
var
  tipoEmpregador: Integer;
begin
   tipoEmpregador:= aTipoEmpregador;
   try
     if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_SetTipoEmpregador(' + IntToStr(aTipoEmpregador) + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_SetTipoEmpregador', logNormal);

        eSocialDM.Travar;
       try
         eSocialDM.ACBreSocial1.Configuracoes.Geral.TipoEmpregador := TEmpregador( tipoEmpregador );
         Result := SetRetorno(ErrOK);
       finally
         eSocialDM.Destravar;
       end;

   except
     on E: EACBrLibException do
     Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

     on E: Exception do
     Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
   end;

end;

function TACBrLibeSocial.SetVersaoDF (const sVersao: PChar):longint;
var
  versao: AnsiString;
begin
  try
    versao:= ConverterAnsiParaUTF8(sVersao);

    if Config.Log.Nivel > logNormal then
      GravarLog('eSocial_SetVersaoDF(' + versao + ' ) ', logCompleto, True)
    else
      GravarLog('eSocial_SetVersaoDF', logNormal);

      eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Configuracoes.Geral.VersaoDF := StrToVersaoeSocialEX(versao);
        Result := SetRetorno(ErrOK);
      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.ConsultaIdentificadoresEventosEmpregador (const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
var
  idEmpregador: String;
  APerApur: TDateTime;
  ATpEvento: Integer;
  AResposta: String;
  Resp : TConsultaTotEventos;
begin
  APerApur:= aPeriodoApuracao;
  aTpEvento:= aTipoEvento;
  idEmpregador:= AnsiString(aIdEmpregador);

  try
    if Config.Log.Nivel > logNormal then
      GravarLog('eSocial_ConsultaIdentificadoresEventosEmpregador(' + aIdEmpregador + ', ' + IntToStr(aTipoEvento) + ',' + DateToStr(aPeriodoApuracao) + ')', logCompleto, True)
    else
      GravarLog('eSocial_ConsultaIdentificadoresEventosEmpregador', logNormal);

      eSocialDM.Travar;
      try

        if ((APerApur <= 0) or (EstaVazio(idEmpregador))) then
         raise EACBrLibException.Create(ErrParametroInvalido, ACBrStr(SErroeSocialConsulta));

        AResposta:= '';
        eSocialDM.ACBreSocial1.Eventos.Clear;
        if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosEmpregador(idEmpregador,
                        TTipoEvento(ATpEvento), APerApur) then
        begin
          Resp := TConsultaTotEventos.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(eSocialDM.ACBreSocial1);
            AResposta := Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(AResposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, AResposta);
        end;

      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.ConsultaIdentificadoresEventosTabela(const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
var
  idEmpregador: String;
  tpEvento: Integer;
  Chave: String;
  DataInicial, DataFinal: TDateTime;
  Resp: TConsultaTotEventos;
  AResposta: String;
begin
  DataInicial:= aDataInicial;
  DataFinal:= aDataFinal;
  Chave:= AnsiString(aChave);
  tpEvento:= aTipoEvento;
  idEmpregador:= AnsiString(aIdEmpregador);

  try
    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_ConsultaIdentificadoresEventosTabela (' + aIdEmpregador + ', ' + IntToStr(aTipoEvento) + ', ' + aChave + ', ' + DateToStr(aDataInicial) + ', ' + DateToStr(aDataFinal) + ')', logCompleto, True)
    else
     GravarLog('eSocial_ConsultaIdentificadoresEventosTabela', logNormal);

      eSocialDM.Travar;
      try
        if ( (EstaVazio(idEmpregador)) or (EstaVazio(Chave)) or (DataInicial <= 0 ) or (DataFinal <= 0) ) then
          raise EACBrLibException.Create(ErrParametroInvalido, ACBrStr(SErroeSocialConsulta));

        AResposta:= '';
        eSocialDM.ACBreSocial1.Eventos.Clear;
        if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosTabela(idEmpregador, TTipoEvento(tpEvento), Chave, DataInicial, DataFinal) then
        begin
          Resp := TConsultaTotEventos.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(eSocialDM.ACBreSocial1);
            AResposta:= Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(AResposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, AResposta);
        end;

      finally
        eSocialDM.Destravar;
      end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibeSocial.ConsultaIdentificadoresEventosTrabalhador (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
var
  idEmpregador: String;
  CPFTrabalhador: String;
  DataInicial, DataFinal: TDateTime;
  Resp: TConsultaTotEventos;
  AResposta: String;
begin
  DataInicial:= aDataInicial;
  DataFinal:= aDataFinal;
  CPFTrabalhador:= AnsiString(aCPFTrabalhador);
  idEmpregador:= AnsiString(aIdEmpregador);

  try
    if Config.Log.Nivel > logNormal then
      GravarLog('eSocial_ConsultaIdentificadoresEventosTrabalhador(' + aIdEmpregador + ', ' + aCPFTrabalhador + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ')', logCompleto, True)
    else
      GravarLog('eSocial_ConsultaIdentificadoresEventosTrabalhador', logNormal);

      eSocialDM.Travar;
      try
        if ((EstaVazio(idEmpregador)) or (EstaVazio(CPFTrabalhador))
           or (DataInicial <= 0)  or (DataFinal <= 0 )) then
          raise EACBrLibException.Create(ErrParametroInvalido, ACBrStr(SErroeSocialConsulta));

        AResposta:= '';
        eSocialDM.ACBreSocial1.Eventos.Clear;
        if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosTrabalhador(idEmpregador,
                        CPFTrabalhador, DataInicial, DataFinal) then
        begin
          Resp := TConsultaTotEventos.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(eSocialDM.ACBreSocial1);
            AResposta:= Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(AResposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, AResposta);
        end;

      finally
        eSocialDM.Destravar;
      end;


  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.DownloadEventos (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
var
  idEmpregador: String;
  AId: String;
  ANrRecibo: String;
  DataInicial, DataFinal: TDateTime;
  Resp: TConsultaEventos;
  AResposta: String;
begin
  idEmpregador:= AnsiString(aIdEmpregador);
  AId:= AnsiString(aCPFTrabalhador);
  DataInicial:= aDataInicial;
  DataFinal:= aDataFinal;
  ANrRecibo:= '';

  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_DownloadEventos(' + aIdEmpregador + ', ' + aCPFTrabalhador + ',' + DateToStr(DataInicial) + ',' + DateToStr(DataFinal) + ')', logCompleto, True)
     else
      GravarLog('eSocial_DownloadEventos', logNormal);

     eSocialDM.Travar;
      try

        if ( (EstaVazio(idEmpregador)) ) then
          raise EACBrLibException.Create(ErrParametroInvalido, ACBrStr(SErroeSocialConsulta));

        AResposta:= '';
        eSocialDM.ACBreSocial1.Eventos.Clear;
        if eSocialDM.ACBreSocial1.DownloadEventos(idEmpregador, AID, ANrRecibo) then
        begin
          Resp := TConsultaEventos.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(eSocialDM.ACBreSocial1);
            AResposta:= Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(AResposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, AResposta);
        end;

      finally
        eSocialDM.Destravar;
      end;


  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibeSocial.ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: Ansistring;
begin
  try
    GravarLog('eSocial_ObterCertificados', logNormal);

    eSocialDM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(eSocialDM.ACBreSocial1.SSL);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      eSocialDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibeSocial.Validar():longint;
begin
  try
    GravarLog('eSocial_Validar', logNormal);

    eSocialDM.Travar;
    try
      try
        eSocialDM.ACBreSocial1.Eventos.Validar;
        Result := SetRetornoeSocialCarregadas(eSocialDM.ACBreSocial1.Eventos.Count);
      except
        on E: EACBreSocialException do
          Result := SetRetorno(ErrValidacaoeSocial, ConverterUTF8ParaAnsi(E.Message));
      end;
    finally
      eSocialDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

