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

unit pnfsGerarNfseResposta;

interface

uses
  SysUtils, Classes, Forms,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pnfsConversao, pnfsNFSe, ACBrUtil, ACBrDFeUtil;

type

 TGerarCompNfseCollection = class;
 TGerarCompNfseCollectionItem = class;
 TGerarMsgRetornoNfseCollection = class;
 TGerarMsgRetornoNfseCollectionItem = class;

 TGerarListaNfse = class(TPersistent)
  private
    FCompNfse : TGerarCompNfseCollection;
    FMsgRetorno : TGerarMsgRetornoNfseCollection;
    procedure SetCompNfse(Value: TGerarCompNfseCollection);
    procedure SetMsgRetorno(Value: TGerarMsgRetornoNfseCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property CompNfse: TGerarCompNfseCollection         read FCompNfse   write SetCompNfse;
    property MsgRetorno: TGerarMsgRetornoNfseCollection read FMsgRetorno write SetMsgRetorno;
  end;

 TGerarCompNfseCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TGerarCompNfseCollectionItem;
    procedure SetItem(Index: Integer; Value: TGerarCompNfseCollectionItem);
  public
    constructor Create(AOwner: TGerarListaNfse);
    function Add: TGerarCompNfseCollectionItem;
    property Items[Index: Integer]: TGerarCompNfseCollectionItem read GetItem write SetItem; default;
  end;

 TGerarCompNfseCollectionItem = class(TCollectionItem)
  private
    FNfse: TNFSe;
    FNfseCancelamento: TConfirmacaoCancelamento;
    FNfseSubstituicao: TSubstituicaoNfse;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Nfse: TNFSe                                read FNfse             write FNfse;
    property NfseCancelamento: TConfirmacaoCancelamento read FNfseCancelamento write FNfseCancelamento;
    property NfseSubstituicao: TSubstituicaoNfse        read FNfseSubstituicao write FNfseSubstituicao;
  end;

 TGerarMsgRetornoNfseCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TGerarMsgRetornoNfseCollectionItem;
    procedure SetItem(Index: Integer; Value: TGerarMsgRetornoNfseCollectionItem);
  public
    constructor Create(AOwner: TGerarListaNfse);
    function Add: TGerarMsgRetornoNfseCollectionItem;
    property Items[Index: Integer]: TGerarMsgRetornoNfseCollectionItem read GetItem write SetItem; default;
  end;

 // Alterado por Nilton Olher - 20/02/2015
 TGerarMsgRetornoNfseIdentificacaoRps = class(TPersistent)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TnfseTipoRps;
  published
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TnfseTipoRps read FTipo write FTipo;
  end;

 TGerarMsgRetornoNfseCollectionItem = class(TCollectionItem)
  private
    FIdentificacaoRps: TGerarMsgRetornoNfseIdentificacaoRps;  // Alterado por Nilton Olher - 20/02/2015
    FCodigo : String;
    FMensagem : String;
    FCorrecao : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property IdentificacaoRps: TGerarMsgRetornoNfseIdentificacaoRps  read FIdentificacaoRps write FIdentificacaoRps;  // Alterado por Nilton Olher - 20/02/2015
    property Codigo: string   read FCodigo   write FCodigo;
    property Mensagem: string read FMensagem write FMensagem;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

 TGerarretNfse = class(TPersistent)
  private
    FPathArquivoMunicipios: string;
    FPathArquivoTabServicos: string;
    FLeitor: TLeitor;
    FListaNfse: TGerarListaNfse;
    FProvedor: TnfseProvedor;
    FTabServicosExt: Boolean;
    FProtocolo: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property PathArquivoMunicipios: string  read FPathArquivoMunicipios  write FPathArquivoMunicipios;
    property PathArquivoTabServicos: string read FPathArquivoTabServicos write FPathArquivoTabServicos;
    property Leitor: TLeitor                read FLeitor                 write FLeitor;
    property ListaNfse: TGerarListaNfse     read FListaNfse              write FListaNfse;
    property Provedor: TnfseProvedor        read FProvedor               write FProvedor;
    property TabServicosExt: Boolean        read FTabServicosExt         write FTabServicosExt;
    property Protocolo: string              read FProtocolo              write FProtocolo;
  end;

implementation

{ TGerarListaNfse }

constructor TGerarListaNfse.Create;
begin
  FCompNfse   := TGerarCompNfseCollection.Create(Self);
  FMsgRetorno := TGerarMsgRetornoNfseCollection.Create(Self);
end;

destructor TGerarListaNfse.Destroy;
begin
  FCompNfse.Free;
  FMsgRetorno.Free;

  inherited;
end;

procedure TGerarListaNfse.SetCompNfse(Value: TGerarCompNfseCollection);
begin
  FCompNfse.Assign(Value);
end;

procedure TGerarListaNfse.SetMsgRetorno(Value: TGerarMsgRetornoNfseCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TGerarCompNfseCollection }

function TGerarCompNfseCollection.Add: TGerarCompNfseCollectionItem;
begin
  Result := TGerarCompNfseCollectionItem(inherited Add);
  Result.create;
end;

constructor TGerarCompNfseCollection.Create(AOwner: TGerarListaNfse);
begin
  inherited Create(TGerarCompNfseCollectionItem);
end;

function TGerarCompNfseCollection.GetItem(
  Index: Integer): TGerarCompNfseCollectionItem;
begin
  Result := TGerarCompNfseCollectionItem(inherited GetItem(Index));
end;

procedure TGerarCompNfseCollection.SetItem(Index: Integer;
  Value: TGerarCompNfseCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TGerarCompNfseCollectionItem }

constructor TGerarCompNfseCollectionItem.Create;
begin
  FNfse             := TNFSe.Create;
  FNfseCancelamento := TConfirmacaoCancelamento.Create;
  FNfseSubstituicao := TSubstituicaoNfse.Create;
end;

destructor TGerarCompNfseCollectionItem.Destroy;
begin
  FNfse.Free;
  FNfseCancelamento.Free;
  FNfseSubstituicao.Free;

  inherited;
end;

{ TGerarMsgRetornoNfseCollection }

function TGerarMsgRetornoNfseCollection.Add: TGerarMsgRetornoNfseCollectionItem;
begin
  Result := TGerarMsgRetornoNfseCollectionItem(inherited Add);
  Result.create;
end;

constructor TGerarMsgRetornoNfseCollection.Create(AOwner: TGerarListaNfse);
begin
  inherited Create(TGerarMsgRetornoNfseCollectionItem);
end;

function TGerarMsgRetornoNfseCollection.GetItem(
  Index: Integer): TGerarMsgRetornoNfseCollectionItem;
begin
  Result := TGerarMsgRetornoNfseCollectionItem(inherited GetItem(Index));
end;

procedure TGerarMsgRetornoNfseCollection.SetItem(Index: Integer;
  Value: TGerarMsgRetornoNfseCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TGerarMsgRetornoNfseCollectionItem }

constructor TGerarMsgRetornoNfseCollectionItem.Create;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps       := TGerarMsgRetornoNfseIdentificacaoRps.Create;
  FIdentificacaoRps.FTipo := trRPS;
end;

destructor TGerarMsgRetornoNfseCollectionItem.Destroy;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps.Free;
  inherited;
end;

{ TGerarretNfse }

constructor TGerarretNfse.Create;
begin
  FLeitor                 := TLeitor.Create;
  FListaNfse              := TGerarListaNfse.Create;
  FPathArquivoMunicipios  := '';
  FPathArquivoTabServicos := '';
end;

destructor TGerarretNfse.Destroy;
begin
  FLeitor.Free;
  FListaNfse.Free;
  inherited;
end;

function TGerarretNfse.LerXml: boolean;
var
  ok: boolean;
  i, k, Item, J: Integer;
  VersaoXML: String;
  ProtocoloTemp, NumeroLoteTemp: String;
  DataRecebimentoTemp:Tdatetime;  
begin
  Result := True;

  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML := NotaUtil.VersaoXML(Leitor.Arquivo);
    k := 0;
    Leitor.Grupo := Leitor.Arquivo;
    // Alterado por Cleiver em 26/02/2013
    if (leitor.rExtrai(1, 'GerarNfseResposta') <> '') or (leitor.rExtrai(1, 'GerarNfseResponse') <> '') or
       (leitor.rExtrai(1, 'EnviarLoteRpsSincronoResposta') <> '') then
    begin
      NumeroLoteTemp:= Leitor.rCampo(tcStr, 'NumeroLote');
      DataRecebimentoTemp:= Leitor.rCampo(tcDatHor, 'DataRecebimento');
      ProtocoloTemp:= Leitor.rCampo(tcStr, 'Protocolo');

      i := 0;
      ListaNfse.FCompNfse.Add;

      ListaNfse.FCompNfse[i].FNfse.NumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
      if trim(ListaNfse.FCompNfse[i].FNfse.NumeroLote) = '' then
        ListaNfse.FCompNfse[i].FNfse.NumeroLote := '0';

      ListaNfse.FCompNfse[i].FNfse.dhRecebimento := Leitor.rCampo(tcDatHor, 'DataRecebimento');

      ListaNfse.FCompNfse[i].FNfse.Protocolo     := Leitor.rCampo(tcStr, 'Protocolo');
      if trim(ListaNfse.FCompNfse[i].FNfse.Protocolo) = '' then
        ListaNfse.FCompNfse[i].FNfse.Protocolo := '0';

      // Ler a Lista de NFSe
      if leitor.rExtrai(2, 'ListaNfse') <> '' then
      begin
        while (Leitor.rExtrai(3, 'CompNfse', '', i + 1) <> '') or
              (Leitor.rExtrai(3, 'ComplNfse', '', i + 1) <> '') or
              ((provedor in [proActcon]) and (Leitor.rExtrai(4, 'Nfse', '', i + 1) <> '')) do
        begin
//          if I > 0 then
            ListaNfse.FCompNfse.Add;

          ListaNfse.FCompNfse[i].FNfse.NumeroLote    := NumeroLoteTemp;
          ListaNfse.FCompNfse[i].FNfse.dhRecebimento := DataRecebimentoTemp;
          ListaNfse.FCompNfse[i].FNfse.Protocolo     := ProtocoloTemp;

          // Grupo da TAG <Nfse> *************************************************
          if Leitor.rExtrai(4, 'Nfse') <> ''
           then begin
            if (provedor in [proActcon]) then
               begin
                    Leitor.rExtrai(4, 'Nfse', '', i + 1);
                    ListaNfse.FCompNfse[i].FNfse.XML := Leitor.Grupo;
               end
            else
            // alterado por joel takei 04/07/2013
            ListaNfse.FCompNfse[i].FNfse.XML := Leitor.rExtrai(4, 'Nfse');

            // Grupo da TAG <InfNfse> *****************************************************
            if Leitor.rExtrai(5, 'InfNfse') <> ''
             then begin
              ListaNfse.FCompNfse[i].FNfse.InfID.ID := trim(Leitor.rAtributo('InfNfse Id='));
              if ListaNfse.FCompNfse[i].FNfse.InfID.ID = ''
               then begin
                ListaNfse.FCompNfse[i].FNfse.InfID.ID := trim(Leitor.rAtributo('InfNfse id='));
                if ListaNfse.FCompNfse[i].FNfse.InfID.ID = ''
                 then ListaNfse.FCompNfse[i].FNfse.InfID.ID := Leitor.rCampo(tcStr, 'Numero');
               end;
              ListaNfse.FCompNfse[i].FNFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
              ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
              if FProvedor in [proFreire, proVitoria]
                then ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao')
                else ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');

              if ListaNfse.FCompNfse[i].FNfse.dhRecebimento = 0 then
                ListaNfse.FCompNfse[i].FNfse.dhRecebimento := ListaNfse.FCompNfse[i].FNFSe.DataEmissao;

              ListaNfse.FCompNfse[i].FNFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
              ListaNfse.FCompNfse[i].FNFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
              ListaNfse.FCompNfse[i].FNFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
              ListaNfse.FCompNfse[i].FNFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));

              ListaNfse.FCompNfse[i].FNFSe.Competencia       := Leitor.rCampo(tcStr, 'Competencia');
              if FProvedor = proISSNet
               then ListaNfse.FCompNfse[i].FNFSe.NfseSubstituida   := ''
               else ListaNfse.FCompNfse[i].FNFSe.NfseSubstituida   := Leitor.rCampo(tcStr, 'NfseSubstituida');
              ListaNfse.FCompNfse[i].FNFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'OutrasInformacoes');
              ListaNfse.FCompNfse[i].FNFSe.ValorCredito      := Leitor.rCampo(tcDe2, 'ValorCredito');

              // Grupo da TAG <IdentificacaoRps> ********************************************
              if Leitor.rExtrai(6, 'IdentificacaoRps') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <RpsSubstituido> **********************************************
              if Leitor.rExtrai(6, 'RpsSubstituido') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <Servico> *****************************************************
              if Leitor.rExtrai(6, 'Servico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico          := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');

                if VersaoXML='1'
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico')
                 else ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

                // Alterado por Cleiver em 26/02/2013
                if (ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico <> '')
                 then begin
                  Item := StrToInt(OnlyNumber(ListaNfse.FCompNfse[i].FNfse.Servico.ItemListaServico));
                  if Item<100 then Item:=Item*100+1;

                  ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
                  ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico :=
                      Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 1, 2) + '.' +
                      Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 3, 2);

                  if TabServicosExt
                   then ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := NotaUtil.ObterDescricaoServico(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico))
                   else ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico));
                 end;

                if length(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 3, 5), 0));
                 (*
                //Alterado por Cleiver em 26/02/2013
                if (ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico <> '')
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico :=
                      CodigoToDesc(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico));
                *)
                if Leitor.rExtrai(7, 'Valores') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
                 end;

                // Incluido por Italo em 27/10/2014
                if ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorLiquidoNfse = 0 then
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorLiquidoNfse := ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorServicos -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.DescontoIncondicionado -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.DescontoCondicionado -
                                                                                   // Retenções Federais
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorPis -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorCofins -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIr -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorInss -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorCsll -

                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.OutrasRetencoes -
                                                                                   ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIssRetido;

                if ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.BaseCalculo = 0 then
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.BaseCalculo := ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorServicos -
                                                                              ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorDeducoes -
                                                                              ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.DescontoIncondicionado;

                if ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIss = 0 then
                  ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.ValorIss := (ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.BaseCalculo *
                                                                            ListaNfse.FCompNfse[i].FNFSe.Servico.Valores.Aliquota)/100;
               end;

              // Grupo da TAG <PrestadorServico> ********************************************
              if Leitor.rExtrai(6, 'PrestadorServico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco = '' then
                begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco := Copy(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Endereco, 11, 125);
                end;
                
                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaoXML='1'
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

                ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoPrestador') <> ''
                 then begin
                  if VersaoXML='1'
                   then begin
                    if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                     then begin
                      ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
                      if ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
                       then ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
                     end;
                   end
                   else ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                 end;

               end;

              // Grupo da TAG <Prestador> ***************************************************
              if Leitor.rExtrai(6, 'Prestador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
                ListaNfse.FCompNfse[i].FNFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
               end;

              // Grupo da TAG <TomadorServico> **********************************************
              if Leitor.rExtrai(6, 'TomadorServico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco = '' then
                begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco, 11, 125);
                end;
                
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaoXML='1'
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

                if ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF = ''
                 then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF := ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.UF;

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <Tomador> *****************************************************
              if Leitor.rExtrai(6, 'Tomador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco = '' then
                begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco := Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Endereco, 11, 125);
                end;
                
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaoXML='1'
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

                if ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF = ''
                 then ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.UF := ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Endereco.UF;

                ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNfse.FCompNfse[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <IntermediarioServico> ****************************************
              if Leitor.rExtrai(6, 'IntermediarioServico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                if Leitor.rExtrai(7, 'CpfCnpj') <> ''
                 then begin
                  if Leitor.rCampo(tcStr, 'Cpf')<>''
                   then ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                   else ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                 end;
               end;

              // Grupo da TAG <OrgaoGerador> ************************************************
              if Leitor.rExtrai(6, 'OrgaoGerador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
               end;

              // Grupo da TAG <ConstrucaoCivil> *********************************************
              if Leitor.rExtrai(6, 'ConstrucaoCivil') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
               end;

              //Grupo da TAG <CondicaoPagamento> ********************************************
              // adicionado por Tailan Bonassi
              if FProvedor in [proBetha] then
               if Leitor.rExtrai(6, 'CondicaoPagamento') <> ''
               then begin
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
                for J := 0 to 9999
                do begin
                 if (Leitor.rExtrai(7, 'Parcelas', 'Parcelas', J) <> '')
                  then begin
                   with ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.Parcelas.Add
                    do begin
                     Parcela        := Leitor.rCampo(tcInt, 'Parcela');
                     DataVencimento := Leitor.rCampo(tcDatVcto, 'DataVencimento');
                     Valor          := Leitor.rCampo(tcDe2, 'Valor');
                   end;
                  end else Break;
                end;
               end;
             end; // fim do InfNfse
           end; // fim do Nfse - Nivel 4


          // Grupo da TAG <NfseCancelamento> ********************************************
          if Leitor.rExtrai(4, 'NfseCancelamento') <> ''
           then begin
            ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');

            // Incluido por Mauro Gomes
            // se não encontrou o campo DataHora, deve procurar pelo DataHoraCancelamento
            if (ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora = 0) then
               ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
           end;

          // Grupo da TAG <NfseSubstituicao> ********************************************
          if Leitor.rExtrai(4, 'NfseSubstituicao') <> ''
           then begin
            ListaNfse.FCompNfse[i].FNfse.NfseSubstituidora := Leitor.rCampo(tcStr, 'NfseSubstituidora');
           end;

          inc(i);
        end; // fim do CompNfse - Nivel 3

      end; // fim do ListaNfse - Nivel 2
      // Ler a Lista de Mensagens
      if (leitor.rExtrai(2, 'ListaMensagemRetorno') <> '') or
         (leitor.rExtrai(2, 'ListaMensagemRetornoLote') <> '') or
         (leitor.rExtrai(2, 'ListaMensagemAlertaRetorno') <> '') or
         (leitor.rExtrai(3, 'Erro') <> '') then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'MensagemRetorno', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
          ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');

          if FProvedor <> proISSe  then // Alterado por Joel Takei Maringá-PR 26/02/2015
            ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(Ok, Leitor.rCampo(tcStr, 'Tipo'));

          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

          inc(i);
        end;

        if FProvedor <> proISSe  then // Alterado por Joel Takei Maringá-PR 26/02/2015
        begin
        
          // Jonatan - Nova Lima MG
          i := 0;
          while Leitor.rExtrai(2, 'ListaMensagemRetorno', '', i + 1) <> '' do
          begin
            ListaNfse.FMsgRetorno.Add;
            ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
            ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
            ListaNfse.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(Ok, Leitor.rCampo(tcStr, 'Tipo'));
            ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
            ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
            ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

            inc(i);
          end;

          // Andeson de Jesus - Luiz Eduardo Magalhães - BA
          i := 0;
          while Leitor.rExtrai(3, 'Erro', '', i + 1) <> '' do
          begin
            ListaNfse.FMsgRetorno.Add;
            ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'ErroID');
            ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'ErroMensagem');
            ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'ErroSolucao');

            inc(i);
          end;
        end; 
        
      end;

    end
    else begin// alterado Joel Takei ISSe 08/06/2013
        if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') or
           (leitor.rExtrai(1, 'ListaMensagemRetornoLote') <> '') then
        begin
          i := 0;
          while Leitor.rExtrai(2, 'MensagemRetorno', '', i + 1) <> '' do
          begin
            ListaNfse.FMsgRetorno.Add;
            ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
            ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
            ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

            inc(i);
          end;
        end;
        // Andeson de Jesus - Barreiras - BA

//        if (leitor.rExtrai(1, 'Fault') <> '') then begin

           i := 0;
           while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
           begin
              ListaNfse.FMsgRetorno.Add;
              ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
              ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
              ListaNfse.FMsgRetorno[i].FCorrecao := '';

              inc(i);
           end;
//        end; //fim Fault

        if (Leitor.rExtrai(1, 'EmitirResponse') <> '') then
        begin
          if Leitor.rCampo(tcStr, 'Erro') <> 'false' then
          begin
            ListaNfse.FCompNfse.Add;
            ListaNfse.FCompNfse[i].FNfse.Numero    := Leitor.rCampo(tcStr, 'b:Numero');
            ListaNfse.FCompNfse[i].FNfse.Protocolo := Leitor.rCampo(tcStr, 'b:Autenticador');
            Protocolo := Leitor.rCampo(tcStr, 'b:Autenticador');
            Result := True;
          end;
        end;
    end;
  except
    result := False;
  end;
end;
end.

