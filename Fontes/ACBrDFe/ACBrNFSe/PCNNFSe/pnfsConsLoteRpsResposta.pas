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

unit pnfsConsLoteRpsResposta;

interface

uses
  SysUtils, Classes, Forms,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pnfsConversao, pnfsNFSe, ACBrUtil, ACBrDFeUtil;

type

 TCompNfseCollection = class;
 TCompNfseCollectionItem = class;
 TMsgRetornoLoteCollection = class;
 TMsgRetornoLoteCollectionItem = class;

 TListaNfse = class(TPersistent)
  private
    FSituacao: string;
    FCompNfse : TCompNfseCollection;
    FMsgRetorno : TMsgRetornoLoteCollection;
    procedure SetCompNfse(Value: TCompNfseCollection);
    procedure SetMsgRetorno(Value: TMsgRetornoLoteCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property Situacao: string                      read FSituacao   write FSituacao;
    property CompNfse: TCompNfseCollection         read FCompNfse   write SetCompNfse;
    property MsgRetorno: TMsgRetornoLoteCollection read FMsgRetorno write SetMsgRetorno;
  end;

 TCompNfseCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCompNfseCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompNfseCollectionItem);
  public
    constructor Create(AOwner: TListaNfse);
    function Add: TCompNfseCollectionItem;
    property Items[Index: Integer]: TCompNfseCollectionItem read GetItem write SetItem; default;
  end;

 TCompNfseCollectionItem = class(TCollectionItem)
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

 TMsgRetornoLoteCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMsgRetornoLoteCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoLoteCollectionItem);
  public
    constructor Create(AOwner: TListaNfse);
    function Add: TMsgRetornoLoteCollectionItem;
    property Items[Index: Integer]: TMsgRetornoLoteCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoLoteCollectionItem = class(TCollectionItem)
  private
    FCodigo : String;
    FMensagem : String;
    FCorrecao : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Codigo: string   read FCodigo   write FCodigo;
    property Mensagem: string read FMensagem write FMensagem;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

 TretLote = class(TPersistent)
  private
    FPathArquivoMunicipios: string;
    FPathArquivoTabServicos: string;
    FLeitor: TLeitor;
    FListaNfse: TListaNfse;
    FProvedor: TnfseProvedor;
    FTabServicosExt: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXml_provedorIssDsf: boolean;
    function LerXML_provedorEquiplano: Boolean;
    function LerXML_provedorEL: Boolean;
    function LerXML_provedorFissLex: Boolean;
  published
    property PathArquivoMunicipios: string  read FPathArquivoMunicipios  write FPathArquivoMunicipios;
    property PathArquivoTabServicos: string read FPathArquivoTabServicos write FPathArquivoTabServicos;
    property Leitor: TLeitor                read FLeitor                 write FLeitor;
    property ListaNfse: TListaNfse          read FListaNfse              write FListaNfse;
    property Provedor: TnfseProvedor        read FProvedor               write FProvedor;
    property TabServicosExt: Boolean        read FTabServicosExt         write FTabServicosExt;
  end;

implementation

{ TListaNfse }

constructor TListaNfse.Create;
begin
  FCompNfse   := TCompNfseCollection.Create(Self);
  FMsgRetorno := TMsgRetornoLoteCollection.Create(Self);
end;

destructor TListaNfse.Destroy;
begin
  if Assigned(FCompNfse)
   then FCompNfse.Free;
  FMsgRetorno.Free;

  inherited;
end;

procedure TListaNfse.SetCompNfse(Value: TCompNfseCollection);
begin
  FCompNfse.Assign(Value);
end;

procedure TListaNfse.SetMsgRetorno(Value: TMsgRetornoLoteCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TCompNfseCollection }

function TCompNfseCollection.Add: TCompNfseCollectionItem;
begin
  Result := TCompNfseCollectionItem(inherited Add);
  Result.create;
end;

constructor TCompNfseCollection.Create(AOwner: TListaNfse);
begin
  inherited Create(TCompNfseCollectionItem);
end;

function TCompNfseCollection.GetItem(
  Index: Integer): TCompNfseCollectionItem;
begin
  Result := TCompNfseCollectionItem(inherited GetItem(Index));
end;

procedure TCompNfseCollection.SetItem(Index: Integer;
  Value: TCompNfseCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCompNfseCollectionItem }

constructor TCompNfseCollectionItem.Create;
begin
  FNfse             := TNFSe.Create;
  FNfseCancelamento := TConfirmacaoCancelamento.Create;
  FNfseSubstituicao := TSubstituicaoNfse.Create;
end;

destructor TCompNfseCollectionItem.Destroy;
begin
  FNfse.Free;
  FNfseCancelamento.Free;
  FNfseSubstituicao.Free;

  inherited;
end;

{ TMsgRetornoLoteCollection }

function TMsgRetornoLoteCollection.Add: TMsgRetornoLoteCollectionItem;
begin
  Result := TMsgRetornoLoteCollectionItem(inherited Add);
  Result.create;
end;

constructor TMsgRetornoLoteCollection.Create(AOwner: TListaNfse);
begin
  inherited Create(TMsgRetornoLoteCollectionItem);
end;

function TMsgRetornoLoteCollection.GetItem(
  Index: Integer): TMsgRetornoLoteCollectionItem;
begin
  Result := TMsgRetornoLoteCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoLoteCollection.SetItem(Index: Integer;
  Value: TMsgRetornoLoteCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TMsgRetornoLoteCollectionItem }

constructor TMsgRetornoLoteCollectionItem.Create;
begin

end;

destructor TMsgRetornoLoteCollectionItem.Destroy;
begin

  inherited;
end;

{ TretLote }

constructor TretLote.Create;
begin
  FLeitor                 := TLeitor.Create;
  FListaNfse              := TListaNfse.Create;
  FPathArquivoMunicipios  := '';
  FPathArquivoTabServicos := '';
end;

destructor TretLote.Destroy;
begin
  FLeitor.Free;
  FListaNfse.Free;
  inherited;
end;

function TretLote.LerXml: boolean;
var
  ok: boolean;
  iNivel,
  i, k, Item, J: Integer;
  VersaodoXML: String;
begin
  result := True;

  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    VersaodoXML    := VersaoXML(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    k      := 0; //length(Prefixo4);
    iNivel := 0;

    // Alterado por Akai - L. Massao Aihara 31/10/2013
    if (leitor.rExtrai(1, 'ConsultarLoteRpsResposta') <> '') or
       (leitor.rExtrai(1, 'Consultarloterpsresposta') <> '') or
       (leitor.rExtrai(1, 'RespostaLoteRps') <> ''         ) or  // Luiz Baião - 2014.12.12_18:04
       (leitor.rExtrai(1, 'ConsultarLoteRpsResult') <> '') then
//    begin
      iNivel := 1;

      // Utilizado pelo provedor fintelISS
      ListaNfse.FSituacao := Leitor.rCampo(tcStr, 'Situacao');
      if ListaNfse.FSituacao = ''
       then ListaNfse.FSituacao := '4';

      // Ler a Lista de NFSe
      if (leitor.rExtrai(iNivel + 1, 'ListaNfse') <> '') or
         (leitor.rExtrai(iNivel + 1, 'CompNfse') <> '') then
      begin
        i := 0;
        // Alterado por Rodrigo Cantelli
        while(Leitor.rExtrai(iNivel + 2, 'CompNfse', '', i + 1) <> '') or
              (Leitor.rExtrai(iNivel + 2, 'ComplNfse', '', i + 1) <> '') or
              (Leitor.rExtrai(iNivel + 2, 'tcCompNfse', '', i + 1) <> '') or
              ((FProvedor in [ProActcon]) and (Leitor.rExtrai(iNivel + 3, 'Nfse', '', i + 1) <> ''))
              do
        begin
          ListaNfse.FCompNfse.Add;

          // Grupo da TAG <Nfse> *************************************************
          if Leitor.rExtrai(iNivel + 3, 'Nfse','') <> ''
           then begin

            if (FProvedor in [ProActcon]) then Leitor.rExtrai(iNivel + 3, 'Nfse', '' , i + 1);

            if Pos('</NFSE>',uppercase(ListaNfse.FCompNfse[i].FNfse.XML))=0 then
               ListaNfse.FCompNfse[i].FNfse.XML:=ListaNfse.FCompNfse[i].FNfse.XML+'</Nfse>';

            ListaNfse.FCompNfse[i].FNfse.XML := {'<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>'+}
                                                Leitor.Grupo {+
                                                '</Nfse>'};
            // Grupo da TAG <InfNfse> *****************************************************
            if Leitor.rExtrai(iNivel + 4, 'InfNfse') <> ''
             then begin
              ListaNfse.FCompNfse[i].FNfse.InfID.ID          := Leitor.rCampo(tcStr, 'Numero');
              ListaNfse.FCompNfse[i].FNFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
              ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');

              if FProvedor in [proSpeedGov, proVitoria, proDBSeller] then
                ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao')
              else
                ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');
				
              // Luiz Baião 2014.12.05 NFSEBrasil   -     <InfNfse Id="049391280001763952720147138">
              if  Leitor.rAtributo('Id') <> EmptyStr then begin
                ListaNfse.FCompNfse[i].FNfse.InfID.ID        := Leitor.rAtributo('Id');
              end;

              // Luiz Baião 2014.12.05 NFSEBrasil   -  <NfseSubstituida>0</NfseSubstituida>
              if (Leitor.rCampo(tcStr, 'NfseSubstituida') <> '0') then begin
               ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'NfseSubstituida');
              end;
			  

              ListaNfse.FCompNfse[i].FNFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
              ListaNfse.FCompNfse[i].FNFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
              ListaNfse.FCompNfse[i].FNFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
              ListaNfse.FCompNfse[i].FNFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
              ListaNfse.FCompNfse[i].FNFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');

              if FProvedor = proISSNet then
                ListaNfse.FCompNfse[i].FNFSe.NfseSubstituida := ''
              else
                ListaNfse.FCompNfse[i].FNFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');

              ListaNfse.FCompNfse[i].FNFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'OutrasInformacoes');
              ListaNfse.FCompNfse[i].FNFSe.ValorCredito      := Leitor.rCampo(tcDe2, 'ValorCredito');

              // Grupo da TAG <IdentificacaoRps> ********************************************
              if Leitor.rExtrai(iNivel + 5, 'IdentificacaoRps') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <RpsSubstituido> **********************************************
              if Leitor.rExtrai(iNivel + 5, 'RpsSubstituido') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <Servico> *****************************************************
              if Leitor.rExtrai(iNivel + 5, 'Servico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico          := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');

                if VersaodoXML='1'
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico')
                 else ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

                Item := StrToInt(OnlyNumber(ListaNfse.FCompNfse[i].FNfse.Servico.ItemListaServico));
                if Item<100 then Item:=Item*100+1;

                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico :=
                    Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 1, 2) + '.' +
                    Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 3, 2);

                if length(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 3, 5), 0));

                if TabServicosExt
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico))
                 else ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico));

                if (Leitor.rExtrai(iNivel + 6, 'Valores') <> '') or (Leitor.rExtrai(iNivel + 6, 'ValoresNfse') <> '')
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
              if Leitor.rExtrai(iNivel + 5, 'PrestadorServico') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoPrestador') <> ''
                 then begin
                  if VersaodoXML='1'
                   then begin
                    if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
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
              if Leitor.rExtrai(iNivel + 5, 'Prestador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
                ListaNfse.FCompNfse[i].FNFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
               end;

              // Grupo da TAG <TomadorServico> **********************************************
              if Leitor.rExtrai(iNivel + 5, 'TomadorServico') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <Tomador> *****************************************************
              if Leitor.rExtrai(iNivel + 5, 'Tomador') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <IntermediarioServico> ****************************************
              if Leitor.rExtrai(iNivel + 5, 'IntermediarioServico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                if Leitor.rExtrai(iNivel + 6, 'CpfCnpj') <> ''
                 then begin
                  if Leitor.rCampo(tcStr, 'Cpf')<>''
                   then ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                   else ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                 end;
               end;

              // Grupo da TAG <OrgaoGerador> ************************************************
              if Leitor.rExtrai(iNivel + 5, 'OrgaoGerador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
               end;

              // Grupo da TAG <ConstrucaoCivil> *********************************************
              if Leitor.rExtrai(iNivel + 5, 'ConstrucaoCivil') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
               end;

              //Grupo da TAG <CondicaoPagamento> ********************************************
              // adicionado por Tailan Bonassi
              if FProvedor in [proBetha] then
               if Leitor.rExtrai(iNivel + 5, 'CondicaoPagamento') <> ''
               then begin
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
                for J := 0 to 9999
                do begin
                 if (Leitor.rExtrai(iNivel + 5, 'Parcelas', 'Parcelas', J) <> '')
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
          if Leitor.rExtrai(iNivel + 3, 'NfseCancelamento') <> ''
           then begin
            ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
            // provedor Betha sempre retorna a o grupo "NfseCancelamento" mesmo não estando cancelada,
            // o cancelamento deverá ser verificado na TAG especifica
            // Incluido por Roberto Godinho 13/11/20113
            if FProvedor = proBetha
             then begin
              Leitor.rExtrai(4,'InfConfirmacaoCancelamento');
              if StrToBool(Leitor.rCampo(tcStr, 'Sucesso'))
               then begin
                ListaNfse.CompNfse[i].NFSe.Status := srCancelado;
                ListaNfse.CompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
               end;
             end else
              begin
                // Incluido por Mauro Gomes
                // se não encontrou o campo DataHora, deve procurar pelo DataHoraCancelamento
                if (ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora = 0) then
                   ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
                if ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora > 0 then
                   ListaNfse.FCompNfse[i].NFSe.Status := srCancelado;
              end;
           end;

          // Grupo da TAG <NfseSubstituicao> ********************************************
          if Leitor.rExtrai(iNivel + 3, 'NfseSubstituicao') <> ''
           then begin
            ListaNfse.FCompNfse[i].FNfse.NfseSubstituidora := Leitor.rCampo(tcStr, 'NfseSubstituidora');
           end;

          inc(i);
        end; // fim do CompNfse - Nivel 3

      end; // fim do ListaNfse - Nivel 2

      // Ler a Lista de Mensagens
      if (leitor.rExtrai(iNivel + 1, 'ListaMensagemRetorno') <> '') or
         (leitor.rExtrai(iNivel + 1, 'Listamensagemretorno') <> '') or
         (leitor.rExtrai(iNivel + 1, 'ListaMensagemAlertaRetorno') <> '') or
         (leitor.rExtrai(iNivel + 1, 'ListaMensagemRetornoLote') <> '') or
         (leitor.rExtrai(iNivel + 1, 'MensagemRetorno') <> '') then
      begin
        i := 0;
        while Leitor.rExtrai(iNivel + IIf(FProvedor in [proSpeedGov], 1, 2), 'MensagemRetorno', '', i + 1) <> '' do begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

          inc(i);
        end;

        if (ListaNfse.FMsgRetorno.Count <= 0) and (FProvedor = proDigifred) then begin
          i := 0;
          while Leitor.rExtrai(iNivel + 2, 'Codigo', '', i + 1) <> '' do begin
            ListaNfse.FMsgRetorno.Add;
            ListaNfse.FMsgRetorno[i].FCodigo := Leitor.rCampo(tcStr, 'Codigo');
            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(iNivel + 2, 'Mensagem', '', i + 1) <> '' do begin
            ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
            inc(i);
          end;

          i := 0;
          while Leitor.rExtrai(iNivel + 2, 'Correcao', '', i + 1) <> '' do begin
            ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
            inc(i);
          end;
        end;
      end;

//    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       ListaNfse.FMsgRetorno.Add;
       ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       ListaNfse.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;

  except
    result := False;
  end;
end;

function TretLote.LerXml_provedorIssDsf: boolean;  //falta homologar
var
  i{, posI, count}: Integer;
  VersaodoXML: String;
//  strAux: AnsiString;
//  leitorAux: TLeitor;
begin
  result := False;

  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    VersaodoXML    := '1';
    Leitor.Grupo   := Leitor.Arquivo;

    if leitor.rExtrai(1, 'RetornoConsultaLote') <> '' then
    begin
      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
         if (Leitor.rCampo(tcStr, 'Sucesso') = 'true') then
         begin
            if (Leitor.rCampo(tcInt, 'QtdNotasProcessadas') > 0) then
            begin
              if leitor.rExtrai(1, 'ListaNFSe') <> '' then
              begin
                i:= 0;
                while leitor.rExtrai(2, 'ConsultaNFSe', '', i + 1) <> '' do
                  begin
                    ListaNfse.FCompNfse.Add;
                    ListaNfse.FCompNfse[i].FNFSe.Numero                 := leitor.rCampo(tcStr, 'NumeroNFe');
                    ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao      := leitor.rCampo(tcStr, 'CodigoVerificacao');
                    ListaNfse.FCompNfse[i].FNFSe.DataEmissao            := leitor.rCampo(tcDatHor, 'DataEmissaoRPS');
                    ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero:= leitor.rCampo(tcStr, 'NumeroRPS');
                    ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Serie := leitor.rCampo(tcStr, 'SerieRPS');
                    inc(i);
                  end;
              end;
            end;
         end;
      end;

      i := 0 ;
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

      i := 0 ;
      if (leitor.rExtrai(2, 'Erros') <> '') then
      begin
        while Leitor.rExtrai(3, 'Erro', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');

          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    result := False;
  end;
end;

function TretLote.LerXML_provedorEquiplano: Boolean;
var
  i: Integer;
begin
  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    if leitor.rExtrai(1, 'listaNfse') <> '' then
      begin
        i:= 0;
        while leitor.rExtrai(2, 'nfse', '', i + 1) <> '' do
          begin
            ListaNfse.FCompNfse.Add;
            ListaNfse.FCompNfse[i].FNFSe.Numero                 := leitor.rCampo(tcStr, 'nrNfse');
            ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao      := leitor.rCampo(tcStr, 'cdAutenticacao');
            ListaNfse.FCompNfse[i].FNFSe.DataEmissao            := leitor.rCampo(tcDatHor, 'dtEmissaoNfs');
            ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero:= leitor.rCampo(tcStr, 'nrRps');
            if Leitor.rExtrai(3, 'cancelamento') <> '' then
            begin
              ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora:= Leitor.rCampo(tcDatHor, 'dtCancelamento');
              ListaNfse.FCompNfse[i].NFSe.MotivoCancelamento       := Leitor.rCampo(tcStr, 'dsCancelamento');
              ListaNfse.FCompNfse[i].NFSe.Status := srCancelado;
            end;
            inc(i);
          end;
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

    Result := True;
  except
    result := False;
  end;
end;

function TretLote.LerXML_provedorEL: Boolean;
var
  i: Integer;
  Cod, Msg: String;
  strAux: AnsiString;
begin
  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    if leitor.rExtrai(1, 'notasFiscais') <> '' then
    begin
      i:= 0;
      while leitor.rExtrai(1, 'notasFiscais', '', i + 1) <> '' do
      begin
        if Leitor.rCampo(tcDatHor, 'dataProcessamento') > 0 then
        begin
          ListaNfse.FCompNfse.Add;
          ListaNfse.FCompNfse[i].FNFSe.Numero                 := leitor.rCampo(tcStr, 'numero');
          ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao      := leitor.rCampo(tcStr, 'idNota');
          ListaNfse.FCompNfse[i].FNFSe.DataEmissao            := leitor.rCampo(tcDatHor, 'dataProcessamento');
          ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero:= leitor.rCampo(tcStr, 'rpsNumero');

          if leitor.rCampo(tcStr, 'idNfseCancelada') <> '' then
          begin
            ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao       := leitor.rCampo(tcStr, 'idNfseCancelada');
            ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora:= Leitor.rCampo(tcDatHor, 'dataProcessamento');
            ListaNfse.FCompNfse[i].NFSe.MotivoCancelamento       := '';
            ListaNfse.FCompNfse[i].NFSe.Status := srCancelado;
          end;
          inc(i);
        end;
      end;
    end;

    if leitor.rExtrai(1, 'mensagens') <> '' then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'mensagens', '', i + 1) <> '' do
      begin
        strAux := Leitor.rCampo(tcStr, 'mensagens');
        Cod    := Copy(strAux, 1, 4);
        Msg    := Copy(strAux, 8, Length(strAux));
        if Trim(Msg) <> '' then
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].Codigo   := Cod;
          ListaNfse.FMsgRetorno[i].Mensagem := Msg;
          Inc(i);
        end else
          Break;
      end;
    end;

    Result := True;
  except
    result := False;
  end;
end;

function TretLote.LerXML_provedorFissLex: Boolean;
var
  ok: boolean;
  iNivel,
  i, k, Item, J: Integer;
  VersaodoXML: String;
begin
  try
    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    k      := 0; //length(Prefixo4);
    iNivel := 0;

    if (leitor.rExtrai(iNivel + 1, 'ListaNfse') <> '') or
          (leitor.rExtrai(iNivel + 1, 'CompNfse') <> '') then
    begin
      i := 0;
      // Alterado por Rodrigo Cantelli
      while(Leitor.rExtrai(iNivel + 2, 'CompNfse', '', i + 1) <> '') or
            (Leitor.rExtrai(iNivel + 2, 'ComplNfse', '', i + 1) <> '') or
            (Leitor.rExtrai(iNivel + 2, 'tcCompNfse', '', i + 1) <> '') or
            ((FProvedor in [ProActcon]) and (Leitor.rExtrai(iNivel + 3, 'Nfse', '', i + 1) <> ''))
            do
      begin
        ListaNfse.FCompNfse.Add;

        // Grupo da TAG <Nfse> *************************************************
        if Leitor.rExtrai(iNivel + 3, 'Nfse','') <> ''
        then
        begin

          if (FProvedor in [ProActcon]) then Leitor.rExtrai(iNivel + 3, 'Nfse', '' , i + 1);

          if Pos('</NFSE>',uppercase(ListaNfse.FCompNfse[i].FNfse.XML))=0 then
            ListaNfse.FCompNfse[i].FNfse.XML:=ListaNfse.FCompNfse[i].FNfse.XML+'</Nfse>';

          ListaNfse.FCompNfse[i].FNfse.XML := {'<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>'+}
                                                Leitor.Grupo {+
                                                '</Nfse>'};
          // Grupo da TAG <InfNfse> *****************************************************
          if Leitor.rExtrai(iNivel + 4, 'InfNfse') <> ''
          then
          begin
            ListaNfse.FCompNfse[i].FNfse.InfID.ID          := Leitor.rCampo(tcStr, 'Numero');
            ListaNfse.FCompNfse[i].FNFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
            ListaNfse.FCompNfse[i].FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');

            if FProvedor in [proSpeedGov, proVitoria, proDBSeller] then
              ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao')
            else
              ListaNfse.FCompNfse[i].FNFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');


            ListaNfse.FCompNfse[i].FNFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
            ListaNfse.FCompNfse[i].FNFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
            ListaNfse.FCompNfse[i].FNFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
            ListaNfse.FCompNfse[i].FNFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
            ListaNfse.FCompNfse[i].FNFSe.Competencia              := Leitor.rCampo(tcStr, 'Competencia');

            ListaNfse.FCompNfse[i].FNFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');

            ListaNfse.FCompNfse[i].FNFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'OutrasInformacoes');
            ListaNfse.FCompNfse[i].FNFSe.ValorCredito      := Leitor.rCampo(tcDe2, 'ValorCredito');

            // Grupo da TAG <IdentificacaoRps> ********************************************
            if Leitor.rExtrai(iNivel + 5, 'IdentificacaoRps') <> ''
            then
            begin
              ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <RpsSubstituido> **********************************************
              if Leitor.rExtrai(iNivel + 5, 'RpsSubstituido') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
                ListaNfse.FCompNfse[i].FNFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
               end;

              // Grupo da TAG <Servico> *****************************************************
              if Leitor.rExtrai(iNivel + 5, 'Servico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico          := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
                ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');

                if VersaodoXML='1'
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico')
                 else ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

                Item := StrToInt(OnlyNumber(ListaNfse.FCompNfse[i].FNfse.Servico.ItemListaServico));
                if Item<100 then Item:=Item*100+1;

                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
                ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico :=
                    Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 1, 2) + '.' +
                    Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico, 3, 2);

                if length(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio)<7
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio :=
                       Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 1, 2) +
                       FormatFloat('00000', StrToIntDef(Copy(ListaNfse.FCompNfse[i].FNFSe.Servico.CodigoMunicipio, 3, 5), 0));

                if TabServicosExt
                 then ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico))
                 else ListaNfse.FCompNfse[i].FNFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(ListaNfse.FCompNfse[i].FNFSe.Servico.ItemListaServico));

                if (Leitor.rExtrai(iNivel + 6, 'Valores') <> '') or (Leitor.rExtrai(iNivel + 6, 'ValoresNfse') <> '')
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
              if Leitor.rExtrai(iNivel + 5, 'PrestadorServico') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoPrestador') <> ''
                 then begin
                  if VersaodoXML='1'
                   then begin
                    if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
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
              if Leitor.rExtrai(iNivel + 5, 'Prestador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
                ListaNfse.FCompNfse[i].FNFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
               end;

              // Grupo da TAG <TomadorServico> **********************************************
              if Leitor.rExtrai(iNivel + 5, 'TomadorServico') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <Tomador> *****************************************************
              if Leitor.rExtrai(iNivel + 5, 'Tomador') <> ''
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

                if VersaodoXML='1'
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

                if Leitor.rExtrai(iNivel + 6, 'Contato') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
                 end;

                if Leitor.rExtrai(iNivel + 6, 'IdentificacaoTomador') <> ''
                 then begin
                  ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                  if Leitor.rExtrai(iNivel + 7, 'CpfCnpj') <> ''
                   then begin
                    if Leitor.rCampo(tcStr, 'Cpf')<>''
                     then ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                     else ListaNfse.FCompNfse[i].FNFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                   end;
                 end;

               end;

              // Grupo da TAG <IntermediarioServico> ****************************************
              if Leitor.rExtrai(iNivel + 5, 'IntermediarioServico') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
                ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
                if Leitor.rExtrai(iNivel + 6, 'CpfCnpj') <> ''
                 then begin
                  if Leitor.rCampo(tcStr, 'Cpf')<>''
                   then ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
                   else ListaNfse.FCompNfse[i].FNFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
                 end;
               end;

              // Grupo da TAG <OrgaoGerador> ************************************************
              if Leitor.rExtrai(iNivel + 5, 'OrgaoGerador') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
                ListaNfse.FCompNfse[i].FNFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
               end;

              // Grupo da TAG <ConstrucaoCivil> *********************************************
              if Leitor.rExtrai(iNivel + 5, 'ConstrucaoCivil') <> ''
               then begin
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
                ListaNfse.FCompNfse[i].FNFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
               end;

              //Grupo da TAG <CondicaoPagamento> ********************************************
              // adicionado por Tailan Bonassi
              if FProvedor in [proBetha] then
               if Leitor.rExtrai(iNivel + 5, 'CondicaoPagamento') <> ''
               then begin
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
                ListaNfse.FCompNfse[i].NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
                for J := 0 to 9999
                do begin
                 if (Leitor.rExtrai(iNivel + 5, 'Parcelas', 'Parcelas', J) <> '')
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
          if Leitor.rExtrai(iNivel + 3, 'NfseCancelamento') <> ''
           then begin
            ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
            // provedor Betha sempre retorna a o grupo "NfseCancelamento" mesmo não estando cancelada,
            // o cancelamento deverá ser verificado na TAG especifica
            // Incluido por Roberto Godinho 13/11/20113
            if FProvedor = proBetha
             then begin
              Leitor.rExtrai(4,'InfConfirmacaoCancelamento');
              if StrToBool(Leitor.rCampo(tcStr, 'Sucesso'))
               then begin
                ListaNfse.CompNfse[i].NFSe.Status := srCancelado;
                ListaNfse.CompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
               end;
              end
             else
              begin
                // Incluido por Mauro Gomes
                // se não encontrou o campo DataHora, deve procurar pelo DataHoraCancelamento
                if (ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora = 0) then
                   ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
                if ListaNfse.FCompNfse[i].NFSe.NfseCancelamento.DataHora > 0 then
                   ListaNfse.FCompNfse[i].NFSe.Status := srCancelado;
              end;
           end;

          // Grupo da TAG <NfseSubstituicao> ********************************************
          if Leitor.rExtrai(iNivel + 3, 'NfseSubstituicao') <> ''
           then begin
            ListaNfse.FCompNfse[i].FNfse.NfseSubstituidora := Leitor.rCampo(tcStr, 'NfseSubstituidora');
           end;

          inc(i);
       end; // fim do CompNfse - Nivel 3

     end; // fim do ListaNfse - Nivel 2

    if leitor.rExtrai(1, 'Listamensagemretorno') <> '' then
    begin
      i := 0;
      if (leitor.rExtrai(2, 'tcMensagemRetorno') <> '') then
      begin
        while Leitor.rExtrai(3, 'tcMensagemRetorno', '', i + 1) <> '' do
        begin
          ListaNfse.FMsgRetorno.Add;
          ListaNfse.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          ListaNfse.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          ListaNfse.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');
          inc(i);
        end;
      end;
    end;

    Result := True;
  except
    result := False;
  end;
end;

end.
