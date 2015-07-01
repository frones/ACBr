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
  SysUtils, Classes, Forms,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pnfsConversao, pnfsNFSeR, ACBrUtil, ACBrDFeUtil;

type

 TLerListaNFSeCollection = class;
 TLerListaNFSeCollectionItem = class;
 TMsgRetornoNFSeCollection = class;
 TMsgRetornoNFSeCollectionItem = class;

 TListaNFSe = class(TPersistent)
  private
    FCompNFSe : TLerListaNFSeCollection;
    FMsgRetorno : TMsgRetornoNFSeCollection;
    procedure SetCompNFSe(Value: TLerListaNFSeCollection);
    procedure SetMsgRetorno(Value: TMsgRetornoNFSeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property CompNFSe: TLerListaNFSeCollection     read FCompNFSe   write SetCompNFSe;
    property MsgRetorno: TMsgRetornoNFSeCollection read FMsgRetorno write SetMsgRetorno;
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

 TMsgRetornoNFSeIdentificacaoRps = class(TPersistent)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TNFSeTipoRps;
  published
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TNFSeTipoRps read FTipo write FTipo;
  end;

 TMsgRetornoNFSeCollectionItem = class(TCollectionItem)
  private
    FIdentificacaoRps: TMsgRetornoNFSeIdentificacaoRps;
    FCodigo : String;
    FMensagem : String;
    FCorrecao : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property IdentificacaoRps: TMsgRetornoNFSeIdentificacaoRps read FIdentificacaoRps write FIdentificacaoRps;
    property Codigo: string   read FCodigo   write FCodigo;
    property Mensagem: string read FMensagem write FMensagem;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

 TRetornoNFSe = class(TPersistent)
  private
    FPathArquivoMunicipios: string;
    FPathArquivoTabServicos: string;
    FLeitor: TLeitor;
    FListaNFSe: TListaNFSe;
    FProvedor: TNFSeProvedor;
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
    property ListaNFSe: TListaNFSe          read FListaNFSe              write FListaNFSe;
    property Provedor: TNFSeProvedor        read FProvedor               write FProvedor;
    property TabServicosExt: Boolean        read FTabServicosExt         write FTabServicosExt;
    property Protocolo: string              read FProtocolo              write FProtocolo;
  end;

implementation

{ TListaNFSe }

constructor TListaNFSe.Create;
begin
  FCompNfse   := TLerListaNFSeCollection.Create(Self);
  FMsgRetorno := TMsgRetornoNFSeCollection.Create(Self);
end;

destructor TListaNFSe.Destroy;
begin
  FCompNfse.Free;
  FMsgRetorno.Free;

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
  FIdentificacaoRps       := TMsgRetornoNFSeIdentificacaoRps.Create;
  FIdentificacaoRps.FTipo := trRPS;
end;

destructor TMsgRetornoNFSeCollectionItem.Destroy;
begin
  FIdentificacaoRps.Free;

  inherited;
end;

{ TRetornoNFSe }

constructor TRetornoNFSe.Create;
begin
  FLeitor                 := TLeitor.Create;
  FListaNfse              := TGerarListaNfse.Create;
  FPathArquivoMunicipios  := '';
  FPathArquivoTabServicos := '';
end;

destructor TRetornoNFSe.Destroy;
begin
  FLeitor.Free;
  FListaNfse.Free;

  inherited;
end;

function TRetornoNFSe.LerXml: boolean;
var
  NFSeLida: TNFSeR;
  VersaodoXML: String;
  ProtocoloTemp, NumeroLoteTemp: String;
  DataRecebimentoTemp:Tdatetime;
begin
  Result := True;

  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    VersaodoXML := VersaoXML(Leitor.Arquivo);
    k := 0;
    Leitor.Grupo := Leitor.Arquivo;

    if (leitor.rExtrai(1, 'GerarNfseResposta') <> '') or
       (leitor.rExtrai(1, 'GerarNfseResponse') <> '') or
       (leitor.rExtrai(1, 'EnviarLoteRpsSincronoResposta') <> '') then
    begin
      // =======================================================================
      // Extrai a Lista de Notas
      // =======================================================================

      NumeroLoteTemp:= Leitor.rCampo(tcStr, 'NumeroLote');
      if trim(NumeroLoteTemp) = '' then
        NumeroLoteTemp := '0';
      DataRecebimentoTemp:= Leitor.rCampo(tcDatHor, 'DataRecebimento');
      ProtocoloTemp:= Leitor.rCampo(tcStr, 'Protocolo');
      if trim(ProtocoloTemp) = '' then
        ProtocoloTemp := '0';

      (*
      with ListaNfse.FCompNfse.Add do
      begin
        FNfse.NumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
        if trim(FNfse.NumeroLote) = '' then
          FNfse.NumeroLote := '0';

        FNfse.dhRecebimento := Leitor.rCampo(tcDatHor, 'DataRecebimento');
        FNfse.Protocolo     := Leitor.rCampo(tcStr, 'Protocolo');
        if trim(FNfse.Protocolo) = '' then
          FNfse.Protocolo := '0';
      end;
      *)

      // Ler a Lista de NFSe
      if leitor.rExtrai(2, 'ListaNfse') <> '' then
      begin
        i := 0;
        while (Leitor.rExtrai(3, 'CompNfse', '', i + 1) <> '') or
              (Leitor.rExtrai(3, 'ComplNfse', '', i + 1) <> '') or
              ((Provedor in [proActcon]) and (Leitor.rExtrai(4, 'Nfse', '', i + 1) <> '')) do
        begin
          // Ler o Grupo da TAG <Nfse> *****************************************
          NFSeLida := TNFSeR.Create;
          try
            NFSeLida.VersaoXML := VersaodoXML;
            NFSeLida.Provedor := Provedor;
            NFSeLida.TabServicosExt := TabServicosExt;
            NFSeLida.Leitor.Arquivo := Leitor.Grupo;

            Result := NFSeLida.LerXml;

            ListaNFSe.FCompNFSe.Add;

            ListaNFSe.FCompNFSe[i].FNFSe.NumeroLote    := NumeroLoteTemp;
            ListaNFSe.FCompNFSe[i].FNFSe.dhRecebimento := DataRecebimentoTemp;
            ListaNFSe.FCompNFSe[i].FNFSe.Protocolo     := ProtocoloTemp;

            ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID := ''; //NFSeLida. . trim(Leitor.rAtributo('InfNFSe Id='));
(*
            // Grupo da TAG <InfNFSe> *****************************************************
            if Leitor.rExtrai(5, 'InfNFSe') <> ''
             then begin
              ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID := trim(Leitor.rAtributo('InfNFSe Id='));
              if ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID = ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID := trim(Leitor.rAtributo('InfNFSe id='));
                if ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID = ''
                 then ListaNFSe.FCompNFSe[i].FNFSe.InfID.ID := Leitor.rCampo(tcStr, 'Numero');
               end;
              ListaNFSe.FCompNFSe[i].FNFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
              ListaNFSe.FCompNFSe[i].FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
              if FProvedor in [proFreire, proVitoria]
                then ListaNFSe.FCompNFSe[i].FNFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao')
                else ListaNFSe.FCompNFSe[i].FNFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');

              if ListaNFSe.FCompNFSe[i].FNFSe.dhRecebimento = 0 then
                ListaNFSe.FCompNFSe[i].FNFSe.dhRecebimento := ListaNFSe.FCompNFSe[i].FNFSe.DataEmissao;

              ListaNFSe.FCompNFSe[i].FNFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
              ListaNFSe.FCompNFSe[i].FNFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
              ListaNFSe.FCompNFSe[i].FNFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
              ListaNFSe.FCompNFSe[i].FNFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));

              ListaNFSe.FCompNFSe[i].FNFSe.Competencia       := Leitor.rCampo(tcStr, 'Competencia');
              if FProvedor = proISSNet
               then ListaNFSe.FCompNFSe[i].FNFSe.NFSeSubstituida   := ''
               else ListaNFSe.FCompNFSe[i].FNFSe.NFSeSubstituida   := Leitor.rCampo(tcStr, 'NFSeSubstituida');
              ListaNFSe.FCompNFSe[i].FNFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'OutrasInformacoes');
              ListaNFSe.FCompNFSe[i].FNFSe.ValorCredito      := Leitor.rCampo(tcDe2, 'ValorCredito');

              // Grupo da TAG <IdentificacaoRps> ********************************************
              if Leitor.rExtrai(6, 'IdentificacaoRps') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNFSe.FCompNFSe[i].FNFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNFSe.FCompNFSe[i].FNFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <RpsSubstituido> **********************************************
              if Leitor.rExtrai(6, 'RpsSubstituido') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNFSe.FCompNFSe[i].FNFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNFSe.FCompNFSe[i].FNFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <Servico> *****************************************************
              if Leitor.rExtrai(6, 'Servico') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico          := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));
                ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
                ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
                ListaNFSe.FCompNFSe[i].FNFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');

                if VersaodoXML='1'
                 then ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico')
                 else ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

                // Alterado por Cleiver em 26/02/2013
                if (ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico <> '')
                 then begin
                  Item := StrToInt(OnlyNumber(ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico));
                  if Item<100 then Item:=Item*100+1;

                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico :=
                      Copy(ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico, 1, 2) + '.' +
                      Copy(ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico, 3, 2);

                  if TabServicosExt
                   then ListaNFSe.FCompNFSe[i].FNFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico))
                   else ListaNFSe.FCompNFSe[i].FNFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(ListaNFSe.FCompNFSe[i].FNFSe.Servico.ItemListaServico));
                 end;

                if length(ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio)<7
                 then ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio :=
                       Copy(ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNFSe.FCompNFSe[i].FNFSe.Servico.CodigoMunicipio, 3, 5), 0));

                if Leitor.rExtrai(7, 'Valores') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorLiquidoNFSe       := Leitor.rCampo(tcDe2, 'ValorLiquidoNFSe');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
                 end;

                // Incluido por Italo em 27/10/2014
                if ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorLiquidoNFSe = 0 then
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorLiquidoNFSe := ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorServicos -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.DescontoIncondicionado -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.DescontoCondicionado -
                                                                                   // Retenções Federais
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorPis -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorCofins -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIr -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorInss -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorCsll -

                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.OutrasRetencoes -
                                                                                   ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIssRetido;

                if ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.BaseCalculo = 0 then
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.BaseCalculo := ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorServicos -
                                                                              ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorDeducoes -
                                                                              ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.DescontoIncondicionado;

                if ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIss = 0 then
                  ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.ValorIss := (ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.BaseCalculo *
                                                                            ListaNFSe.FCompNFSe[i].FNFSe.Servico.Valores.Aliquota)/100;
               end;

              // Grupo da TAG <PrestadorServico> ********************************************
              if Leitor.rExtrai(6, 'PrestadorServico') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco = '' then
                begin
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco := Copy(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Endereco, 11, 125);
                end;
                
                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaodoXML='1'
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio)<7
                 then ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio :=
                       Copy(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

                ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoPrestador') <> ''
                 then begin
                  if VersaodoXML='1'
                   then begin
                    if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                     then begin
                      ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
                      if ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = ''
                       then ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
                     end;
                   end
                   else ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
                  ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                 end;

               end;

              // Grupo da TAG <Prestador> ***************************************************
              if Leitor.rExtrai(6, 'Prestador') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
                ListaNFSe.FCompNFSe[i].FNFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
               end;

              // Grupo da TAG <TomadorServico> **********************************************
              if Leitor.rExtrai(6, 'TomadorServico') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco = '' then
                begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco, 11, 125);
                end;
                
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaodoXML='1'
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio)<7
                 then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio :=
                       Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

                if ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF = ''
                 then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF := ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.UF;

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <Tomador> *****************************************************
              if Leitor.rExtrai(6, 'Tomador') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
                if ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco = '' then
                begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
                  if Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco, 1, 10 + k) = '<' + 'Endereco>'
                   then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco := Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Endereco, 11, 125);
                end;
                
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

                if VersaodoXML='1'
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Estado');
                 end
                 else begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
                 end;

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

                if length(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio)<7
                 then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio :=
                       Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

                if ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF = ''
                 then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.UF := ListaNFSe.FCompNFSe[i].FNFSe.PrestadorServico.Endereco.UF;

                ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Endereco.CodigoMunicipio, 0));

                if Leitor.rExtrai(7, 'Contato') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(7, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(8, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNFSe.FCompNFSe[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <IntermediarioServico> ****************************************
              if Leitor.rExtrai(6, 'IntermediarioServico') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNFSe.FCompNFSe[i].FNFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                if Leitor.rExtrai(7, 'CpfCnpj') <> ''
                 then begin
                  if Leitor.rCampo(tcStr, 'Cpf')<>''
                   then ListaNFSe.FCompNFSe[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                   else ListaNFSe.FCompNFSe[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                 end;
               end;

              // Grupo da TAG <OrgaoGerador> ************************************************
              if Leitor.rExtrai(6, 'OrgaoGerador') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                ListaNFSe.FCompNFSe[i].FNFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
               end;

              // Grupo da TAG <ConstrucaoCivil> *********************************************
              if Leitor.rExtrai(6, 'ConstrucaoCivil') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].FNFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
                ListaNFSe.FCompNFSe[i].FNFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
               end;

              //Grupo da TAG <CondicaoPagamento> ********************************************
              // adicionado por Tailan Bonassi
              if FProvedor in [proBetha] then
               if Leitor.rExtrai(6, 'CondicaoPagamento') <> ''
               then begin
                ListaNFSe.FCompNFSe[i].NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
                ListaNFSe.FCompNFSe[i].NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
                for J := 0 to 9999
                do begin
                 if (Leitor.rExtrai(7, 'Parcelas', 'Parcelas', J) <> '')
                  then begin
                   with ListaNFSe.FCompNFSe[i].NFSe.CondicaoPagamento.Parcelas.Add
                    do begin
                     Parcela        := Leitor.rCampo(tcInt, 'Parcela');
                     DataVencimento := Leitor.rCampo(tcDatVcto, 'DataVencimento');
                     Valor          := Leitor.rCampo(tcDe2, 'Valor');
                   end;
                  end else Break;
                end;
               end;
             end; // fim do InfNFSe
           end; // fim do NFSe - Nivel 4


          // Grupo da TAG <NFSeCancelamento> ********************************************
          if Leitor.rExtrai(4, 'NFSeCancelamento') <> ''
           then begin
            ListaNFSe.FCompNFSe[i].NFSe.NFSeCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');

            // Incluido por Mauro Gomes
            // se não encontrou o campo DataHora, deve procurar pelo DataHoraCancelamento
            if (ListaNFSe.FCompNFSe[i].NFSe.NFSeCancelamento.DataHora = 0) then
               ListaNFSe.FCompNFSe[i].NFSe.NFSeCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
           end;

          // Grupo da TAG <NFSeSubstituicao> ********************************************
          if Leitor.rExtrai(4, 'NFSeSubstituicao') <> ''
           then begin
            ListaNFSe.FCompNFSe[i].FNFSe.NFSeSubstituidora := Leitor.rCampo(tcStr, 'NFSeSubstituidora');
           end;
*)
            end;
          finally
             NFSeLida.Free;
          end;
        end;
      end;
    end
    else begin
      // =======================================================================
      // Extrai a Lista de Mensagens de Erro
      // =======================================================================

      if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') or
         (leitor.rExtrai(1, 'ListaMensagemRetornoLote') <> '') then
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

      i := 0;
      while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
      begin
        ListaNFSe.FMsgRetorno.Add;
        ListaNFSe.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
        ListaNFSe.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
        ListaNFSe.FMsgRetorno[i].FCorrecao := '';

        inc(i);
      end;

      if (Leitor.rExtrai(1, 'EmitirResponse') <> '') then
      begin
        if Leitor.rCampo(tcStr, 'Erro') <> 'false' then
        begin
          ListaNFSe.FCompNFSe.Add;
          ListaNFSe.FCompNFSe[i].FNFSe.Numero    := Leitor.rCampo(tcStr, 'b:Numero');
          ListaNFSe.FCompNFSe[i].FNFSe.Protocolo := Leitor.rCampo(tcStr, 'b:Autenticador');
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
