{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{$I ACBr.inc}

unit pnfsLerListaNFSe;

interface

uses
  SysUtils, Classes, Forms, variants,
  pcnConversao, pcnLeitor,
  pnfsConversao, pnfsNFSe, pnfsNFSeR, ACBrUtil;

type

  TLerListaNFSeCollection = class;
  TLerListaNFSeCollectionItem = class;
  TMsgRetornoNFSeCollection = class;
  TMsgRetornoNFSeCollectionItem = class;

 TListaNFSe = class(TPersistent)
  private
    FCompNFSe: TLerListaNFSeCollection;
    FMsgRetorno: TMsgRetornoNFSeCollection;
    FSucesso: String;
    FChaveNFeRPS: TChaveNFeRPS;

    procedure SetCompNFSe(Value: TLerListaNFSeCollection);
    procedure SetMsgRetorno(Value: TMsgRetornoNFSeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property CompNFSe: TLerListaNFSeCollection     read FCompNFSe        write SetCompNFSe;
    property MsgRetorno: TMsgRetornoNFSeCollection read FMsgRetorno      write SetMsgRetorno;
    property Sucesso: String                       read FSucesso         write FSucesso;
    property ChaveNFeRPS: TChaveNFeRPS             read FChaveNFeRPS     write FChaveNFeRPS;
  end;

 TLerListaNFSeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLerListaNFSeCollectionItem;
    procedure SetItem(Index: Integer; Value: TLerListaNFSeCollectionItem);
  public
    constructor Create(AOwner: TListaNFSe);
    function Add: TLerListaNFSeCollectionItem;
    property Items[Index: Integer]: TLerListaNFSeCollectionItem read GetItem write SetItem; default;
  end;

 TLerListaNFSeCollectionItem = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FNFSeCancelamento: TConfirmacaoCancelamento;
    FNFSeSubstituicao: TSubstituicaoNFSe;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property NFSe: TNFSe                                read FNFSe             write FNFSe;
    property NFSeCancelamento: TConfirmacaoCancelamento read FNFSeCancelamento write FNFSeCancelamento;
    property NFSeSubstituicao: TSubstituicaoNFSe        read FNFSeSubstituicao write FNFSeSubstituicao;
  end;

 TMsgRetornoNFSeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMsgRetornoNFSeCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoNFSeCollectionItem);
  public
    constructor Create(AOwner: TListaNFSe);
    function Add: TMsgRetornoNFSeCollectionItem;
    property Items[Index: Integer]: TMsgRetornoNFSeCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoNFSeCollectionItem = class(TCollectionItem)
  private
    FCodigo: String;
    FMensagem: String;
    FCorrecao: String;
    FIdentificacaoRps: TMsgRetornoIdentificacaoRps;
    FChaveNFeRPS: TChaveNFeRPS;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Codigo: String   read FCodigo   write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property Correcao: String read FCorrecao write FCorrecao;
    property IdentificacaoRps: TMsgRetornoIdentificacaoRps read FIdentificacaoRps write FIdentificacaoRps;
    property ChaveNFeRPS: TChaveNFeRPS read FChaveNFeRPS write FChaveNFeRPS;
  end;

 TRetornoNFSe = class(TPersistent)
  private
    FLeitor: TLeitor;
    FListaNFSe: TListaNFSe;
    FProvedor: TNFSeProvedor;
    FTabServicosExt: Boolean;
    FProtocolo: String;
    FSituacao: String;
    FPathIniCidades: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor         read FLeitor         write FLeitor;
    property ListaNFSe: TListaNFSe   read FListaNFSe      write FListaNFSe;
    property Provedor: TNFSeProvedor read FProvedor       write FProvedor;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt;
    property Protocolo: String       read FProtocolo      write FProtocolo;
    property Situacao:string         read FSituacao       write FSituacao;
    property PathIniCidades: String  read FPathIniCidades write FPathIniCidades;
  end;

implementation

{ TListaNFSe }

constructor TListaNFSe.Create;
begin
  FCompNfse   := TLerListaNFSeCollection.Create(Self);
  FMsgRetorno := TMsgRetornoNFSeCollection.Create(Self);
  FChaveNFeRPS := TChaveNFeRPS.Create;
end;

destructor TListaNFSe.Destroy;
begin
  FCompNfse.Free;
  FMsgRetorno.Free;
  FChaveNFeRPS.Free;

  inherited;
end;

procedure TListaNFSe.SetCompNFSe(Value: TLerListaNFSeCollection);
begin
  FCompNfse.Assign(Value);
end;

procedure TListaNFSe.SetMsgRetorno(Value: TMsgRetornoNFSeCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TLerListaNFSeCollection }

function TLerListaNFSeCollection.Add: TLerListaNFSeCollectionItem;
begin
  Result := TLerListaNFSeCollectionItem(inherited Add);
  Result.create;
end;

constructor TLerListaNFSeCollection.Create(AOwner: TListaNFSe);
begin
  inherited Create(TLerListaNFSeCollectionItem);
end;

function TLerListaNFSeCollection.GetItem(
  Index: Integer): TLerListaNFSeCollectionItem;
begin
  Result := TLerListaNFSeCollectionItem(inherited GetItem(Index));
end;

procedure TLerListaNFSeCollection.SetItem(Index: Integer;
  Value: TLerListaNFSeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TLerListaNFSeCollectionItem }

constructor TLerListaNFSeCollectionItem.Create;
begin
  FNfse             := TNFSe.Create;
  FNfseCancelamento := TConfirmacaoCancelamento.Create;
  FNfseSubstituicao := TSubstituicaoNfse.Create;
end;

destructor TLerListaNFSeCollectionItem.Destroy;
begin
  FNfse.Free;
  FNfseCancelamento.Free;
  FNfseSubstituicao.Free;

  inherited;
end;

{ TMsgRetornoNFSeCollection }

function TMsgRetornoNFSeCollection.Add: TMsgRetornoNFSeCollectionItem;
begin
  Result := TMsgRetornoNFSeCollectionItem(inherited Add);
  Result.create;
end;

constructor TMsgRetornoNFSeCollection.Create(AOwner: TListaNFSe);
begin
  inherited Create(TMsgRetornoNFSeCollectionItem);
end;

function TMsgRetornoNFSeCollection.GetItem(
  Index: Integer): TMsgRetornoNFSeCollectionItem;
begin
  Result := TMsgRetornoNFSeCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoNFSeCollection.SetItem(Index: Integer;
  Value: TMsgRetornoNFSeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TMsgRetornoNFSeCollectionItem }

constructor TMsgRetornoNFSeCollectionItem.Create;
begin
  FIdentificacaoRps := TMsgRetornoIdentificacaoRps.Create;
  FIdentificacaoRps.Tipo := trRPS;
  FChaveNFeRPS := TChaveNFeRPS.Create;
end;

destructor TMsgRetornoNFSeCollectionItem.Destroy;
begin
  FIdentificacaoRps.Free;
  FChaveNFeRPS.Free;

  inherited;
end;

{ TRetornoNFSe }

constructor TRetornoNFSe.Create;
begin
  FLeitor    := TLeitor.Create;
  FListaNfse := TListaNfse.Create;
  FProtocolo := '';
  FSituacao  := '';
end;

destructor TRetornoNFSe.Destroy;
begin
  FLeitor.Free;
  FListaNfse.Free;

  inherited;
end;

function TRetornoNFSe.LerXml: Boolean;
var
  NFSe: TNFSe;
  NFSeLida: TNFSeR;
  VersaodoXML, Msg, ProtocoloTemp, NumeroLoteTemp, SituacaoTemp: String;
  DataRecebimentoTemp: TDateTime;
  i, j, Nivel, MsgErro: Integer;
  Nivel1: Boolean;
  lNFSe: TLerListaNFSeCollectionItem;
begin
  Result := True;

  try
    if Provedor = proISSCuritiba then
      Leitor.Arquivo := RemoverNameSpace(Leitor.Arquivo)
    else
      Leitor.Arquivo := RemoverNameSpace(RemoverAtributos(RetirarPrefixos(Leitor.Arquivo, Provedor), Provedor));

    VersaodoXML  := VersaoXML(Leitor.Arquivo);
    Leitor.Grupo := Leitor.Arquivo;

    Nivel1 := (leitor.rExtrai(1, 'GerarNfseResposta') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'GerarNfseResponse') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RecepcionarLoteRpsResult') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'EnviarLoteRpsSincronoResposta') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarLoteRpsResposta') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarLoteRpsResult') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfseRpsResposta') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfsePorRpsResult') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfseResposta') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfseResponse') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfseFaixaResposta') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ConsultarNfseServicoPrestadoResponse') <> '');
      
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'resPedidoLoteNFSe') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'CancelarNfseResult') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RetornoConsultaRPS') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'listaNfse') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'ListaNfse') <> '');
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'nfse') <> '');

    // Assessor Publico
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'NFSE') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RetornoConsultaLote') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RetornoConsultaNFSeRPS') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RetornoEnvioRPS') <> '');

    //Conam
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'Sdt_consultanotasprotocoloout') <> '');

    //fiss-lex
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'WS_ConsultaNfsePorRps.ExecuteResponse') <> '');

    //GOVERNA
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RecepcionarConsultaNotaCanceladaResult') <> '');

    //NFSeBrasil
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RespostaLoteRps') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RespostaConsultaNFSE') <> '');

    //SP
    if not Nivel1 then
      Nivel1 := (Leitor.rExtrai(1, 'RetornoConsulta') <> '');

    //CTA
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'RetornoConsultaNotas') <> '');

    //EL
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'notasFiscais') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'nfeRpsNotaFiscal') <> '');

    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'nfdok') <> '');

    //IPM
    if not Nivel1 then
      Nivel1 := (leitor.rExtrai(1, 'retorno') <> '');

    //DataSmart
    if ((not Nivel1) and (Provedor = proDataSmart)) then
      Nivel1 := (leitor.rExtrai(1, 'CompNfse') <> '');

    if Nivel1 then
    begin
      // =======================================================================
      // Extrai a Lista de Notas
      // =======================================================================

      NumeroLoteTemp:= Leitor.rCampo(tcStr, 'NumeroLote');
      if trim(NumeroLoteTemp) = '' then
        NumeroLoteTemp := '0';

      DataRecebimentoTemp:= Leitor.rCampo(tcDatHor, 'DataRecebimento');
      if (DataRecebimentoTemp = 0) then
        DataRecebimentoTemp:= Leitor.rCampo(tcDatHor, 'DataEnvioLote');

      ProtocoloTemp:= Leitor.rCampo(tcStr, 'Protocolo');
      if (Provedor in [ProNFSeBrasil]) and (AnsiUpperCase(ProtocoloTemp) = 'NAO FOI GERADO NUMERO DE PROTOCOLO PARA ESSA TRANSACAO.') then
        ProtocoloTemp := '';
      if trim(ProtocoloTemp) = '' then
        ProtocoloTemp := '0';

//      if (Provedor in [ProTecnos]) and (ProtocoloTemp <> '') then
      FProtocolo := ProtocoloTemp;

      SituacaoTemp:= Leitor.rCampo(tcStr, 'Situacao');
      if trim(SituacaoTemp) = '' then
        SituacaoTemp := '4';
      FSituacao := SituacaoTemp;

      // Ler a Lista de NFSe
      if leitor.rExtrai(2, 'ListaNfse') <> '' then
        Nivel := 3
      else if leitor.rExtrai(2, 'notas') <> '' then
        Nivel := 3
      else
        Nivel := 2;

      // GOVERNA
      if leitor.rExtrai(2,'RetornoConsultaCancelamento') <> '' then
      begin
        Nivel := 3;
        DataRecebimentoTemp := Date;
      end;

      // SP
      if (leitor.rExtrai(2, 'NFe') <> '') then
      begin
        Nivel := 2;
        DataRecebimentoTemp := Leitor.rCampo(tcDatHor, 'DataEmissaoNFe');
      end;

      if ((Provedor = proDataSmart) and (leitor.rExtrai(1, 'NFSe') <> '')) then
        Nivel := 1;

      i := 0;
      while (Leitor.rExtrai(Nivel, 'tcCompNfse', '', i + 1) <> '') or
            (Leitor.rExtrai(Nivel, 'CompNfse', '', i + 1) <> '') or
            (Leitor.rExtrai(Nivel, 'ComplNfse', '', i + 1) <> '') or
            (leitor.rExtrai(Nivel, 'RetornoConsultaRPS', '', i + 1) <> '') or
            (leitor.rExtrai(Nivel, 'NFe', '', i + 1) <> '') or                   // Provedor SP
            (leitor.rExtrai(Nivel, 'Reg20Item', '', i + 1) <> '') or             // Provedor CONAM
            ((Provedor in [proActcon]) and (Leitor.rExtrai(Nivel + 1, 'Nfse', '', i + 1) <> '')) or
            ((Provedor in [proAgili, proAgiliv2, proDataSmart]) and (Leitor.rExtrai(Nivel, 'Nfse', '', i + 1) <> '')) or
            ((Provedor in [proEquiplano]) and (Leitor.rExtrai(Nivel, 'nfse', '', i + 1) <> '')) or
            ((Provedor in [proNFSeBrasil]) and (Leitor.rExtrai(Nivel, 'nota', '', i + 1) <> '')) or
            ((Provedor in [proISSDSF]) and (Leitor.rExtrai(Nivel, 'ConsultaNFSe', '', i + 1) <> '')) or     // ConsultaLote
            ((Provedor in [proISSDSF]) and (Leitor.rExtrai(Nivel, 'NotasConsultadas', '', i + 1) <> '')) or // ConsultaNFSePorRPS
            ((Provedor in [proInfisc, proInfiscv11]) and (Leitor.rExtrai(Nivel, 'resPedidoLoteNFSe', '', i + 1) <> '')) or
            ((Provedor in [proGoverna]) and (Leitor.rExtrai(Nivel, 'InfRetConsultaNotCan', '', i + 1) <> '')) or
            ((Provedor in [proCTA]) and (Leitor.rExtrai(Nivel, 'Nota', '', i + 1) <> '')) or
            ((Provedor in [proEL]) and (Leitor.rExtrai(Nivel, 'notasFiscais', '', i + 1) <> '')) or // ConsultaLote
            ((Provedor in [proEL]) and (Leitor.rExtrai(Nivel, 'nfeRpsNotaFiscal', '', i + 1) <> '')) or // ConsultaNFSePorRPS
            ((Provedor in [proSMARAPD]) and (Leitor.rExtrai(Nivel, 'nfdok', '', i + 1) <> '')) or
            ((Provedor in [proAssessorPublico]) and (Leitor.rExtrai(Nivel, 'NOTA', '', i + 1) <> '')) or
            ((Provedor in [proIPM]) and (Leitor.rExtrai(Nivel, 'nfse', '', i + 1) <> '')) do
      begin
        NFSe := TNFSe.Create;
        NFSeLida := TNFSeR.Create(NFSe);
        try
          NFSeLida.VersaoXML      := VersaodoXML;
          NFSeLida.Provedor       := Provedor;
          NFSeLida.TabServicosExt := TabServicosExt;
          NFSeLida.PathIniCidades := PathIniCidades;

          NFSeLida.Leitor.Arquivo := Leitor.Grupo;

          Result := NFSeLida.LerXml;

          if Result then
          begin
            with ListaNFSe.FCompNFSe.Add do
            begin
              // Armazena o XML da NFS-e
              FNFSe.XML := SeparaDados(Leitor.Grupo, 'tcCompNfse');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'CompNfse');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'ComplNfse');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'RetornoConsultaRPS');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'NFe');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'Nfse');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'nfse');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'ConsultaNFSe');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'NotasConsultadas');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'NFS-e');
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'Reg20Item');  // Provedor Conam
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'xml');  // Provedor NFSeBrasil
              if NFSe.XML = '' then
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'Nota', True);
//              if NFSe.XML = '' then
//                FNFSe.XML := SeparaDados(Leitor.Grupo, 'tbnfd', True);

              if Provedor = proSMARAPD then
                FNFSe.XML := Leitor.Grupo;

              if (Provedor = proAssessorPublico) and (NFSe.XML = '') then
                FNFSe.XML := Leitor.Grupo;

              if (Provedor = proEL) then
              begin
                FNFSe.XML := SeparaDados(Leitor.Grupo, 'notasFiscais', True);
                if NFSe.XML = '' then
                  FNFSe.XML := SeparaDados(Leitor.Grupo, 'nfeRpsNotaFiscal', True);
              end;

              // Retorno do GerarNfse e EnviarLoteRpsSincrono

              // Roberto Godinho - Para o provedor CTA deve ler o Numero do Lote
              // do arquivo de cada nota quando for Consulta por Periodo
              if Provedor = proCTA then
              begin
                NumeroLoteTemp := Leitor.rCampo(tcStr, 'NumeroLote');
                DataRecebimentoTemp := Leitor.rCampo(tcDat, 'DataProcessamento');
              end;

              FNFSe.NumeroLote    := NumeroLoteTemp;

              if FNFSe.NumeroLote = '0' then
                FNFSe.NumeroLote := NFSeLida.NFSe.NumeroLote;

              FNFSe.dhRecebimento := DataRecebimentoTemp;
              FNFSe.Protocolo     := ProtocoloTemp;

              if FNFSe.Protocolo = '0' then
                FNFSe.Protocolo := Protocolo;

              // Retorno do ConsultarLoteRps
              FNFSe.Situacao := SituacaoTemp;

              {Márcio - Se tiver link na NFSeLida, então deve ser tratado lá, e aqui simplesmente pega o valor }
              FNFSe.Link              := NFSeLida.NFSe.Link;

              FNFSe.InfID.ID          := NFSeLida.NFSe.InfID.ID;
              FNFSe.Numero            := NFSeLida.NFSe.Numero;
              FNFSe.SeriePrestacao    := NFSeLida.NFSe.SeriePrestacao;
              FNFSe.CodigoVerificacao := NFSeLida.NFSe.CodigoVerificacao;
              FNFSe.DataEmissao       := NFSeLida.NFSe.DataEmissao;
              FNFSe.DataEmissaoRps    := NFSeLida.NFSe.DataEmissaoRps;
              FNFSe.dhRecebimento     := NFSeLida.NFSe.dhRecebimento;

              FNFSe.NaturezaOperacao         := NFSeLida.NFSe.NaturezaOperacao;
              FNFSe.RegimeEspecialTributacao := NFSeLida.NFSe.RegimeEspecialTributacao;
              FNFSe.OptanteSimplesNacional   := NFSeLida.NFSe.OptanteSimplesNacional;
              FNFSe.IncentivadorCultural     := NFSeLida.NFSe.IncentivadorCultural;

              FNFSe.Competencia       := NFSeLida.NFSe.Competencia;
              FNFSe.NFSeSubstituida   := NFSeLida.NFSe.NFSeSubstituida;
              FNFSe.OutrasInformacoes := NFSeLida.NFSe.OutrasInformacoes;
              FNFSe.ValorCredito      := NFSeLida.NFSe.ValorCredito;

              FNFSe.ValoresNfse.BaseCalculo      := NFSeLida.NFSe.ValoresNfse.BaseCalculo;
              FNFSe.ValoresNfse.Aliquota         := NFSeLida.NFSe.ValoresNfse.Aliquota;
              FNFSe.ValoresNfse.ValorIss         := NFSeLida.NFSe.ValoresNfse.ValorIss;
              FNFSe.ValoresNfse.ValorLiquidoNfse := NFSeLida.NFSe.ValoresNfse.ValorLiquidoNfse;

              FNFSe.IdentificacaoRps.Numero := NFSeLida.NFSe.IdentificacaoRps.Numero;
              FNFSe.IdentificacaoRps.Serie  := NFSeLida.NFSe.IdentificacaoRps.Serie;
              FNFSe.IdentificacaoRps.Tipo   := NFSeLida.NFSe.IdentificacaoRps.Tipo;

              FNFSe.RpsSubstituido.Numero := NFSeLida.NFSe.RpsSubstituido.Numero;
              FNFSe.RpsSubstituido.Serie  := NFSeLida.NFSe.RpsSubstituido.Serie;
              FNFSe.RpsSubstituido.Tipo   := NFSeLida.NFSe.RpsSubstituido.Tipo;

              FNFSe.Servico.ItemListaServico          := NFSeLida.NFSe.Servico.ItemListaServico;
              FNFSe.Servico.xItemListaServico         := NFSeLida.NFSe.Servico.xItemListaServico;
              FNFSe.Servico.CodigoCnae                := NFSeLida.NFSe.Servico.CodigoCnae;
              FNFSe.Servico.CodigoTributacaoMunicipio := NFSeLida.NFSe.Servico.CodigoTributacaoMunicipio;
              FNFSe.Servico.Discriminacao             := NFSeLida.NFSe.Servico.Discriminacao;
              FNFSe.Servico.CodigoMunicipio           := NFSeLida.NFSe.Servico.CodigoMunicipio;
              FNFSe.Servico.ExigibilidadeISS          := NFSeLida.NFSe.Servico.ExigibilidadeISS;

              FNFSe.Servico.Valores.ValorServicos          := NFSeLida.NFSe.Servico.Valores.ValorServicos;
              FNFSe.Servico.Valores.ValorDeducoes          := NFSeLida.NFSe.Servico.Valores.ValorDeducoes;
              FNFSe.Servico.Valores.ValorPis               := NFSeLida.NFSe.Servico.Valores.ValorPis;
              FNFSe.Servico.Valores.ValorCofins            := NFSeLida.NFSe.Servico.Valores.ValorCofins;
              FNFSe.Servico.Valores.ValorInss              := NFSeLida.NFSe.Servico.Valores.ValorInss;
              FNFSe.Servico.Valores.ValorIr                := NFSeLida.NFSe.Servico.Valores.ValorIr;
              FNFSe.Servico.Valores.ValorCsll              := NFSeLida.NFSe.Servico.Valores.ValorCsll;
              FNFSe.Servico.Valores.IssRetido              := NFSeLida.NFSe.Servico.Valores.IssRetido;
              FNFSe.Servico.Valores.ValorIss               := NFSeLida.NFSe.Servico.Valores.ValorIss;
              FNFSe.Servico.Valores.OutrasRetencoes        := NFSeLida.NFSe.Servico.Valores.OutrasRetencoes;
              FNFSe.Servico.Valores.BaseCalculo            := NFSeLida.NFSe.Servico.Valores.BaseCalculo;
              FNFSe.Servico.Valores.Aliquota               := NFSeLida.NFSe.Servico.Valores.Aliquota;
              FNFSe.Servico.Valores.ValorLiquidoNFSe       := NFSeLida.NFSe.Servico.Valores.ValorLiquidoNFSe;
              FNFSe.Servico.Valores.ValorIssRetido         := NFSeLida.NFSe.Servico.Valores.ValorIssRetido;
              FNFSe.Servico.Valores.DescontoCondicionado   := NFSeLida.NFSe.Servico.Valores.DescontoCondicionado;
              FNFSe.Servico.Valores.DescontoIncondicionado := NFSeLida.NFSe.Servico.Valores.DescontoIncondicionado;

              FNFSe.Prestador.Cnpj               := NFSeLida.NFSe.Prestador.Cnpj;
              FNFSe.Prestador.InscricaoMunicipal := NFSeLida.NFSe.Prestador.InscricaoMunicipal;

              FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := NFSeLida.NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
              FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSeLida.NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

              FNFSe.PrestadorServico.RazaoSocial  := NFSeLida.NFSe.PrestadorServico.RazaoSocial;
              FNFSe.PrestadorServico.NomeFantasia := NFSeLida.NFSe.PrestadorServico.NomeFantasia;

              FNFSe.PrestadorServico.Endereco.Endereco    := NFSeLida.NFSe.PrestadorServico.Endereco.Endereco;
              FNFSe.PrestadorServico.Endereco.Numero      := NFSeLida.NFSe.PrestadorServico.Endereco.Numero;
              FNFSe.PrestadorServico.Endereco.Complemento := NFSeLida.NFSe.PrestadorServico.Endereco.Complemento;
              FNFSe.PrestadorServico.Endereco.Bairro      := NFSeLida.NFSe.PrestadorServico.Endereco.Bairro;

              FNFSe.PrestadorServico.Endereco.CodigoMunicipio := NFSeLida.NFSe.PrestadorServico.Endereco.CodigoMunicipio;
              FNFSe.PrestadorServico.Endereco.xMunicipio := NFSeLida.NFSe.PrestadorServico.Endereco.xMunicipio;

              FNFSe.PrestadorServico.Endereco.UF  := NFSeLida.NFSe.PrestadorServico.Endereco.UF;
              FNFSe.PrestadorServico.Endereco.CEP := NFSeLida.NFSe.PrestadorServico.Endereco.CEP;

              FNFSe.PrestadorServico.Contato.Telefone := NFSeLida.NFSe.PrestadorServico.Contato.Telefone;
              FNFSe.PrestadorServico.Contato.Email    := NFSeLida.NFSe.PrestadorServico.Contato.Email;

              FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := NFSeLida.NFSe.Tomador.IdentificacaoTomador.CpfCnpj;
              FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := NFSeLida.NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal;

              FNFSe.Tomador.RazaoSocial := NFSeLida.NFSe.Tomador.RazaoSocial;

              FNFSe.Tomador.Endereco.Endereco    := NFSeLida.NFSe.Tomador.Endereco.Endereco;
              FNFSe.Tomador.Endereco.Numero      := NFSeLida.NFSe.Tomador.Endereco.Numero;
              FNFSe.Tomador.Endereco.Complemento := NFSeLida.NFSe.Tomador.Endereco.Complemento;
              FNFSe.Tomador.Endereco.Bairro      := NFSeLida.NFSe.Tomador.Endereco.Bairro;

              FNFSe.Tomador.Endereco.CodigoMunicipio := NFSeLida.NFSe.Tomador.Endereco.CodigoMunicipio;
              FNFSe.Tomador.Endereco.xMunicipio      := NFSeLida.NFSe.Tomador.Endereco.xMunicipio;
              FNFSe.Tomador.Endereco.UF              := NFSeLida.NFSe.Tomador.Endereco.UF;

              FNFSe.Tomador.Endereco.CEP := NFSeLida.NFSe.Tomador.Endereco.CEP;

              FNFSe.Tomador.Contato.Telefone := NFSeLida.NFSe.Tomador.Contato.Telefone;
              FNFSe.Tomador.Contato.Email    := NFSeLida.NFSe.Tomador.Contato.Email;

              FNFSe.IntermediarioServico.CpfCnpj := NFSeLida.NFSe.IntermediarioServico.CpfCnpj;
              FNFSe.IntermediarioServico.InscricaoMunicipal := NFSeLida.NFSe.IntermediarioServico.InscricaoMunicipal;
              FNFSe.IntermediarioServico.RazaoSocial := NFSeLida.NFSe.IntermediarioServico.RazaoSocial;

              FNFSe.OrgaoGerador.CodigoMunicipio := NFSeLida.NFSe.OrgaoGerador.CodigoMunicipio;
              FNFSe.OrgaoGerador.Uf              := NFSeLida.NFSe.OrgaoGerador.Uf;

              FNFSe.ConstrucaoCivil.CodigoObra := NFSeLida.NFSe.ConstrucaoCivil.CodigoObra;
              FNFSe.ConstrucaoCivil.Art        := NFSeLida.NFSe.ConstrucaoCivil.Art;

              FNFSe.CondicaoPagamento.Condicao   := NFSeLida.NFSe.CondicaoPagamento.Condicao;
              FNFSe.CondicaoPagamento.QtdParcela := NFSeLida.NFSe.CondicaoPagamento.QtdParcela;

              FNFSe.NFSeCancelamento.DataHora := NFSeLida.NFSe.NFSeCancelamento.DataHora;
              FNFSe.NFSeCancelamento.Pedido.CodigoCancelamento := NFSeLida.NFSe.NFSeCancelamento.Pedido.CodigoCancelamento;
              FNFSe.Cancelada := NFSeLida.NFSe.Cancelada;
              FNFSe.Status := NFSeLida.NFSe.Status;

              FNFSe.NFSeSubstituidora := NFSeLida.NFSe.NFSeSubstituidora;
            end;

            for j := 0 to NFSeLida.NFSe.CondicaoPagamento.Parcelas.Count -1 do
            begin
              with ListaNFSe.FCompNFSe[i].FNFSe.CondicaoPagamento.Parcelas.New do
              begin
                Parcela        := NFSeLida.NFSe.CondicaoPagamento.Parcelas.Items[j].Parcela;
                DataVencimento := NFSeLida.NFSe.CondicaoPagamento.Parcelas.Items[j].DataVencimento;
                Valor          := NFSeLida.NFSe.CondicaoPagamento.Parcelas.Items[j].Valor;
              end;
            end;
          end;
        finally
           NFSeLida.Free;
           NFSe.Free;
        end;

        inc(i); // Incrementa o contador de notas.
      end;

      if (Provedor = ProTecnos) then
      begin
        if (ListaNFSe.CompNFSe.Count = 0) then
          lNFSe := ListaNFSe.CompNFSe.Add
        else
          lNFSe := ListaNFSe.CompNFSe.Items[0];

        lNFSe.NFSe.NumeroLote    := NumeroLoteTemp;
        lNFSe.NFSe.dhRecebimento := DataRecebimentoTemp;
        lNFSe.NFSe.Protocolo     := ProtocoloTemp;

        if (NumeroLoteTemp = '0') or (ProtocoloTemp = '0') then
          Result := False;
      end;

      // Provedor CTA - Retorno da situação do lote
      if (Provedor = proCTA) and (Leitor.rExtrai(1, 'RetornoConsultaLote') <> '') then
      begin
        Protocolo                                := Leitor.rCampo(tcStr, 'NumeroLote'); // retorna o protocolo no numero do lote
        ListaNFSe.ChaveNFeRPS.Numero             := Leitor.rCampo(tcStr, 'NumeroRPS');
        ListaNFSe.ChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
        ListaNFSe.ChaveNFeRPS.CodigoVerificacao  := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        ListaNFSe.ChaveNFeRPS.SerieRPS           := Leitor.rCampo(tcStr, 'SerieRPS');
        ListaNFSe.ChaveNFeRPS.NumeroRPS          := Leitor.rCampo(tcStr, 'NumeroRPS');
        ListaNFSe.Sucesso                        := Leitor.rCampo(tcStr, 'Sucesso');
        Result := ListaNFSe.Sucesso = 'true';
      end;

      if Provedor = proISSDigital then
      begin
        i := 0;
        while Leitor.rExtrai(2, 'ListaMensagemRetorno', '', i + 1) <> '' do
        begin
          ListaNFSe.FMsgRetorno.Add;
          ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
          inc(i);
        end;
      end;
    end;

    // =======================================================================
    // Extrai a Lista de Mensagens de Erro
    // =======================================================================

    Leitor.Grupo := Leitor.Arquivo;

    if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') or
       (leitor.rExtrai(1, 'Listamensagemretorno') <> '') or
       (leitor.rExtrai(1, 'ListaMensagemRetornoLote') <> '') then
    begin
      i := 0;
      while Leitor.rExtrai(2, 'MensagemRetorno', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

        if FProvedor = proPronimv2 then
          if (leitor.rExtrai(3, 'IdentificacaoRps') <> '') then
            ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');

        inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'MensagemRetornoLote', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
        inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'tcMensagemRetorno', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

        inc(i);
      end;
    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
    begin
      ListaNFSe.FMsgRetorno.Add;
      ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
      ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
      ListaNFSe.FMsgRetorno[i].FCorrecao := '';

      inc(i);
    end;

    if leitor.rExtrai(1, 'mensagemRetorno') <> '' then
    begin
      i := 0;
      if (leitor.rExtrai(2, 'listaErros') <> '') then
      begin
        while Leitor.rExtrai(3, 'erro', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
          ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

          inc(i);
        end;
      end;

      if (leitor.rExtrai(2, 'listaAlertas') <> '') then
      begin
        while Leitor.rExtrai(3, 'alerta', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
          ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

          inc(i);
        end;
      end;
    end;

    i := 0;
    if (leitor.rExtrai(2, 'Alertas') <> '') then
    begin
      while Leitor.rExtrai(3, 'Alerta', '', i + 1) <> '' do
      begin
        ListaNfse.FMsgRetorno.Add;
        ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');

        inc(i);
      end;
    end;

    i := 0;
    if (leitor.rExtrai(2, 'Erros') <> '') then
    begin
      while Leitor.rExtrai(3, 'Erro', '', i + 1) <> '' do
      begin
        ListaNfse.FMsgRetorno.Add;
        ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
        // Roberto Godinho - Provedor CTA pode retornar erros de schema substituindo a TAG <descricao> por <erro>
        // se não tratado resulta em exception vazio.
        if ListaNfse.FMsgRetorno[i].FMensagem = '' then
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Erro');

        if FProvedor = proIssDSF then
        begin
          if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
          begin
            ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
            ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
            ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
          end;
        end;

        inc(i);
      end;
    end;

    if FProvedor = proNFSeBrasil then
    begin
      i := 0;
      if (leitor.rExtrai(2, 'erros') <> '') then
      begin
        while Leitor.rExtrai(3, 'erro', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'erro');

          inc(i);
        end;
      end;
    end;

    if FProvedor = proGoverna then
    begin
      j := 0;
      MsgErro := 0;
      while Leitor.rExtrai(2, 'DesOco', '', j + 1) <> '' do
      begin
        Msg  := Leitor.rCampo(tcStr, 'DesOco');
        if (Pos('OK!', Msg) = 0) and (Pos('RPS já Importado', Msg) = 0) and
           (Pos('Sucesso', Msg) = 0) then
        begin
          ListaNFSe.FMsgRetorno.Add;
          ListaNFSe.FMsgRetorno[MsgErro].FMensagem := Msg;
          inc(MsgErro);
        end;
        inc(j);
      end;

      MsgErro := 0;
      j := 0;
      while Leitor.rExtrai(1, 'InfRetConsultaNotCan', '', j+1) <> '' do
      begin
        Msg  := Leitor.rCampo(tcStr, 'SitCan');
        if Msg = 'N' then
          Msg := 'Nota não cancelada!'
        else
          Msg := 'Nota cancelada!';

        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[MsgErro].FMensagem := Msg;
        inc(MsgErro);

        Msg  := Leitor.rCampo(tcStr, 'DatCan');
        if Msg <> '' then
        begin
          ListaNFSe.FMsgRetorno.Add;
          ListaNFSe.FMsgRetorno[MsgErro].FMensagem := 'Data de Cancelamento: ' + Msg;
        end;
        inc(j);
      end;

      Result := true;
      
      // Bloco abaixo verificar a real necessidade
      With ListaNFSe.FCompNFSe.Add do
      begin
        FNFSe.dhRecebimento := Date;
      end;
    end;

    if FProvedor = proEGoverneISS then
    begin
      i := 0;
      if (Leitor.rExtrai(1, 'EmitirResponse') <> '') then
      begin
        if Leitor.rCampo(tcStr, 'Erro') <> 'false' then
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := 'Erro';
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'MensagemErro');
        end
        else
        begin
          with ListaNFSe.FCompNFSe.Add do
          begin
            FNFSe.Autenticador := Leitor.rCampo(tcStr, 'Autenticador');
            FNFSe.Link := Leitor.rCampo(tcStr, 'Link');
            FNFSe.Numero := Leitor.rCampo(tcStr, 'Numero');
          end;
        end;
      end;
    end;

    if FProvedor in [proSP, proNotaBlu, proISSDSF] then
    begin
      try
        if (Leitor.rExtrai(1, 'RetornoConsulta') <> '') or
           (Leitor.rExtrai(1, 'RetornoConsultaLote') <> '') or
           (Leitor.rExtrai(1, 'RetornoEnvioRPS') <> '') then
        begin
          ListaNFSe.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');

          i := 0;
          while Leitor.rExtrai(2, 'Alerta', '', i + 1) <> '' do
          begin
            ListaNFSe.FMsgRetorno.Add;
            ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
            ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
            ListaNFSe.FMsgRetorno[i].FCorrecao := '';

            if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
            begin
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
            end;

            if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
            begin
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
            end;

            Inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(2, 'Erro', '', i + 1) <> '' do
          begin
            ListaNFSe.MsgRetorno.Add;
            ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
            ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
            ListaNFSe.FMsgRetorno[i].FCorrecao := '';

            if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
            begin
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
            end;

            if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
            begin
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
              ListaNFSe.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
            end;

            Inc(i);
          end;

          Result := True;
        end;
      except
        Result := False;
      end;
    end;

    if FProvedor in [proNotaBlu] then
    begin
      try
        if Leitor.rExtrai(1, 'RetornoEnvioRPS') <> '' then
          ListaNFSe.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
        Result := True;
      except
        Result := False;
      end;
    end;

    if FProvedor = proCONAM then
    begin
      if leitor.rExtrai(2, 'Messages') <> '' then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'Message', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Id');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Description');
          Inc(i);
        end;
      end;
    end;

    if FProvedor = proSpeedGov then
    begin
      i := 0;
      while Leitor.rExtrai(2, 'MensagemRetorno', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
        inc(i);
      end;
    end;

    if FProvedor = proCoplan then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'MensagemRetorno', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        ListaNFSe.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
        inc(i);
      end;
    end;

    if FProvedor = proEL then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'mensagens', '', i + 1) <> '' do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'mensagens');
        Inc(i);
      end;
    end;

    if FProvedor in [proIPM] then
    begin
      try
        if( Leitor.rExtrai( 1, 'retorno' ) <> '' )then
        begin
          if( Leitor.rExtrai( 2, 'mensagem' ) <> '' )then
          begin
            if( Copy( Leitor.rCampo( tcStr, 'codigo' ), 1, 5 ) <> '00001' )then
            begin
              i := 0;
              while Leitor.rExtrai(3, 'codigo', '', i + 1 ) <> '' do
              begin
                ListaNfse.FMsgRetorno.Add;
                ListaNfse.FMsgRetorno[i].FCodigo   := Copy( Leitor.rCampo( tcStr, 'codigo' ), 1, 5 );
                ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo( tcStr, 'codigo' );

                Inc(i);
              end;
            end;
          end;
        end;

        if( ListaNfse.FMsgRetorno.Count = 0 )then
        begin
          if (Leitor.rExtrai(1, 'retorno') <> '') then
          begin
            ListaNFSe.FChaveNFeRPS.Numero            := Leitor.rCampo(tcStr, 'numero_nfse');
            ListaNFSe.FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'cod_verificador_autenticidade');

            //ListaNFSe.FCompNFSe.Items[0].FNFSe.XML;
            if (ListaNFSe.CompNFSe.Count = 0) then
              lNFSe := ListaNFSe.CompNFSe.Add
            else
              lNFSe := ListaNFSe.CompNFSe.Items[0];

            lNFSe.NFSe.InfID.ID                := Leitor.rCampo( tcStr, 'numero_nfse' );
            lNFSe.NFSe.Numero                  := Leitor.rCampo( tcStr, 'numero_nfse' );
            lNFSe.NFSe.DataEmissao             := StrToDateDef( VarToStr(Leitor.rCampo(tcStr, 'data_nfse')), 0) +
                                                  StrToTimeDef( VarToStr(Leitor.rCampo(tcStr, 'hora_nfse')), 0);
            lNFSe.NFSe.Competencia             := DateToStr( lNFSe.NFSe.DataEmissao );
            lNFSe.NFSe.dhRecebimento           := lNFSe.NFSe.DataEmissao;
            lNFSe.NFSe.Protocolo               := Leitor.rCampo(tcStr, 'cod_verificador_autenticidade');
            lNFSe.NFSe.Link                    := Leitor.rCampo(tcStr, 'link_nfse');
            lNFSe.NFSe.CodigoVerificacao       := Leitor.rCampo(tcStr, 'cod_verificador_autenticidade');
            lNFSe.NFSe.XML                     := Leitor.Arquivo;// FListaNFSe.FCompNFSe.Items[0].FNFSe.XML;

            if (Leitor.rCampo(tcStr, 'situacao_descricao_nfse') = 'Emitida') then
            begin
              lNFSe.NFSe.Cancelada := snNao;
              lNFSe.NFSe.Status := srNormal;
            end
            else
            begin
              if (Leitor.rCampo(tcStr, 'situacao_descricao_nfse') = 'Cancelada') then
              begin
                lNFSe.NFSe.Cancelada := snSim;
                lNFSe.NFSe.Status := srCancelado;
              end;
            end;

            Result := True;
          end;

          if (leitor.rExtrai(2, 'rps') <> '') then
          begin
            if (ListaNFSe.CompNFSe.Count = 0) then
              lNFSe := ListaNFSe.CompNFSe.Add
            else
              lNFSe := ListaNFSe.CompNFSe.Items[0];

            lNFSe.NFSe.IdentificacaoRps.Numero := Leitor.rCampo( tcStr, 'nro_recibo_provisorio' );
            lNFSe.NFSe.IdentificacaoRps.Serie  := Leitor.rCampo( tcStr, 'serie_recibo_provisorio' );

            ListaNFSe.FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'nro_recibo_provisorio');
            ListaNFSe.FChaveNFeRPS.SerieRPS  := Leitor.rCampo(tcStr, 'serie_recibo_provisorio');
          end;
        end;
      except
          Result := False;
      end;
    end;

    {Provedor Giap}
    if FProvedor = proGiap then
    begin
      if Leitor.rExtrai(1, 'consultaResposta') <> EmptyStr then
      begin
        if Leitor.rCampo(tcStr, 'notaExiste') = 'Sim' then
        begin
          ListaNFSe.ChaveNFeRPS.CodigoVerificacao  := Leitor.rCampo(tcStr, 'codigoVerificacao');
          ListaNFSe.ChaveNFeRPS.Numero             := Leitor.rCampo(tcStr, 'numeroNota');
          ListaNFSe.ChaveNFeRPS.Link               := '';
          ListaNFSe.Sucesso                        := 'True';
        end
        else
        begin
          with ListaNFSe.MsgRetorno.Add do
          begin
            Codigo := '0';
            Mensagem:= 'Nota Não Existe';
          end;
          Result := False;
        end;
      end;
    end;

  except
    Result := False;
  end;
end;

end.
