{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreRetReceita;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnLeitor, pgnreConfigUF, pgnreRetProduto,
  pgnreRetDetalhamentoReceita, pgnreRetPeriodoApuracao,
  pgnreRetTipoDocumentoOrigem, pgnreRetCampoAdicional;

type
  TRetInfReceitaCollection = class;
  TRetInfReceitaCollectionItem = class;
  TRetReceita = class;

  TRetInfReceitaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfReceitaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfReceitaCollectionItem);
  public
    function Add: TRetInfReceitaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfReceitaCollectionItem;
    property Items[Index: Integer]: TRetInfReceitaCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfReceitaCollectionItem = class(TObject)
  private
    FRetInfReceita: TRetInfReceita;
    FretDetalhamentoReceita: TRetInfDetalhamentoReceitaCollection;
    FretProduto: TRetInfProdutoCollection;
    FretPeriodoApuracao: TRetInfPeriodoApuracaoCollection;
    FretTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection;
    FretCampoAdicional: TRetInfCampoAdicionalCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property RetInfReceita: TRetInfReceita read FRetInfReceita write FRetInfReceita;
    property retDetalhamentoReceita: TRetInfDetalhamentoReceitaCollection read FretDetalhamentoReceita write FretDetalhamentoReceita;
    property retProduto: TRetInfProdutoCollection read FretProduto write FretProduto;
    property retPeriodoApuracao: TRetInfPeriodoApuracaoCollection read FretPeriodoApuracao write FretPeriodoApuracao;
    property retTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection read FretTipoDocumentoOrigem write FretTipoDocumentoOrigem;
    property retCampoAdicional: TRetInfCampoAdicionalCollection read FretCampoAdicional write FretCampoAdicional;
  end;

  TRetReceita = class(TObject)
  private
    FLeitor: TLeitor;
    FretReceita: TRetInfReceitaCollection;
    FInfDetalhamentoReceita: TRetDetalhamentoReceita;
    FInfProduto: TRetProduto;
    FInfPeriodoApuracao: TRetPeriodoApuracao;
    FInfTipoDocumentoOrigem: TRetTipoDocumentoOrigem;
    FInfCampoAdicional: TRetCampoAdicional;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property retReceita: TRetInfReceitaCollection read FretReceita write FretReceita;
    property InfDetalhamentoReceita: TRetDetalhamentoReceita read FInfDetalhamentoReceita write FInfDetalhamentoReceita;
    property InfProduto: TRetProduto read FInfProduto write FInfProduto;
    property InfPeriodoApuracao: TRetPeriodoApuracao read FInfPeriodoApuracao write FInfPeriodoApuracao;
    property InfTipoDocumentoOrigem: TRetTipoDocumentoOrigem read FInfTipoDocumentoOrigem write FInfTipoDocumentoOrigem;
    property InfCampoAdicional: TRetCampoAdicional read FInfCampoAdicional write FInfCampoAdicional;
  end;

implementation

uses
  ACBrUtil.XMLHTML;

{ TRetInfReceitaCollection }

function TRetInfReceitaCollection.Add: TRetInfReceitaCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfReceitaCollection.GetItem(
  Index: Integer): TRetInfReceitaCollectionItem;
begin
  Result := TRetInfReceitaCollectionItem(inherited Items[Index]);
end;

function TRetInfReceitaCollection.New: TRetInfReceitaCollectionItem;
begin
  Result := TRetInfReceitaCollectionItem.Create();
  Self.Add(Result);
end;

procedure TRetInfReceitaCollection.SetItem(Index: Integer;
  Value: TRetInfReceitaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetInfReceitaCollectionItem }

constructor TRetInfReceitaCollectionItem.Create;
begin
  FRetInfReceita := TRetInfReceita.Create;
  FretDetalhamentoReceita := TRetInfDetalhamentoReceitaCollection.Create;
  FretProduto := TRetInfProdutoCollection.Create;
  FretPeriodoApuracao := TRetInfPeriodoApuracaoCollection.Create;
  FretTipoDocumentoOrigem := TRetInfTipoDocumentoOrigemCollection.Create;
  FretCampoAdicional := TRetInfCampoAdicionalCollection.Create;
end;

destructor TRetInfReceitaCollectionItem.Destroy;
begin
  FRetInfReceita.Free;
  FretDetalhamentoReceita.Free;
  FretProduto.Free;
  FretPeriodoApuracao.Free;
  FretTipoDocumentoOrigem.Free;
  FretCampoAdicional.Free;

  inherited;
end;

{ TRetReceita }

constructor TRetReceita.Create;
begin
  FLeitor := TLeitor.Create;
  FretReceita := TRetInfReceitaCollection.Create;
end;

destructor TRetReceita.Destroy;
begin
  FLeitor.Free;
  FretReceita.Free;

  if Assigned(FInfDetalhamentoReceita) then
    FInfDetalhamentoReceita.Free;

  if Assigned(FInfProduto) then
    FInfProduto.Free;

  if Assigned(FInfPeriodoApuracao) then
    FInfPeriodoApuracao.Free;

  if Assigned(FInfTipoDocumentoOrigem) then
    FInfTipoDocumentoOrigem.Free;

  if Assigned(FInfCampoAdicional) then
    FInfCampoAdicional.Free;

  inherited;
end;

function TRetReceita.LerXml: Boolean;
var
  i, j: Integer;
  DetalhamentoReceita: TRetInfDetalhamentoReceitaCollectionItem;
  Produto: TRetInfProdutoCollectionItem;
  PeriodoApuracao: TRetInfPeriodoApuracaoCollectionItem;
  TipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollectionItem;
  CampoAdicional: TRetInfCampoAdicionalCollectionItem;
begin
  Result := False;

  try
    i := 0;
    if Leitor.rExtrai(1, 'ns1:receitas') <> '' then
    begin
      while Leitor.rExtrai(2, 'ns1:receita', '', i + 1) <> '' do
      begin
        FretReceita.New;
        FretReceita.Items[i].RetInfReceita.codigo    := Leitor.rAtributo('codigo');
        FretReceita.Items[i].RetInfReceita.descricao := Leitor.rAtributo('descricao');

        if Pos('courier', Leitor.Grupo) > 0 then
          FretReceita.Items[i].RetInfReceita.courier := Leitor.rAtributo('courier');

        FretReceita.Items[i].RetInfReceita.exigeContribuinteEmitente     := SeparaDados(Leitor.Grupo, 'ns1:exigeContribuinteEmitente');
        FretReceita.Items[i].RetInfReceita.exigeDetalhamentoReceita      := SeparaDados(Leitor.Grupo, 'ns1:exigeDetalhamentoReceita');
        FretReceita.Items[i].RetInfReceita.exigeProduto                  := SeparaDados(Leitor.Grupo, 'ns1:exigeProduto');
        FretReceita.Items[i].RetInfReceita.exigePeriodoReferencia        := SeparaDados(Leitor.Grupo, 'ns1:exigePeriodoReferencia');
        FretReceita.Items[i].RetInfReceita.exigePeriodoApuracao          := SeparaDados(Leitor.Grupo, 'ns1:exigePeriodoApuracao');
        FretReceita.Items[i].RetInfReceita.exigeParcela                  := SeparaDados(Leitor.Grupo, 'ns1:exigeParcela');
        FretReceita.Items[i].RetInfReceita.valorExigido                  := SeparaDados(Leitor.Grupo, 'ns1:valorExigido');
        FretReceita.Items[i].RetInfReceita.exigeDocumentoOrigem          := SeparaDados(Leitor.Grupo, 'ns1:exigeDocumentoOrigem');
        FretReceita.Items[i].RetInfReceita.exigeContribuinteDestinatario := SeparaDados(Leitor.Grupo, 'ns1:exigeContribuinteDestinatario');
        FretReceita.Items[i].RetInfReceita.exigeDataVencimento           := SeparaDados(Leitor.Grupo, 'ns1:exigeDataVencimento');
        FretReceita.Items[i].RetInfReceita.exigeDataPagamento            := SeparaDados(Leitor.Grupo, 'ns1:exigeDataPagamento');
        FretReceita.Items[i].RetInfReceita.exigeConvenio                 := SeparaDados(Leitor.Grupo, 'ns1:exigeConvenio');
        FretReceita.Items[i].RetInfReceita.exigeCamposAdicionais         := SeparaDados(Leitor.Grupo, 'ns1:exigeCamposAdicionais');

        if Assigned(FInfDetalhamentoReceita) then
          FInfDetalhamentoReceita.Free;

        FInfDetalhamentoReceita := TRetDetalhamentoReceita.Create;
        
        if SameText(retReceita.Items[i].RetInfReceita.exigeDetalhamentoReceita, 'S') then
        begin
          InfDetalhamentoReceita.Leitor.Arquivo := FLeitor.Grupo;
          InfDetalhamentoReceita.LerXml;

          if InfDetalhamentoReceita.retDetalhamentoReceita.Count > 0 then
          begin
            for j := 0 to InfDetalhamentoReceita.retDetalhamentoReceita.Count - 1 do
            begin
              DetalhamentoReceita := FretReceita.Items[i].FretDetalhamentoReceita.New;
              DetalhamentoReceita.RetDetalhamentoReceita.codigo :=
                InfDetalhamentoReceita.retDetalhamentoReceita.Items[j].RetDetalhamentoReceita.codigo;
              DetalhamentoReceita.RetDetalhamentoReceita.descricao :=
                InfDetalhamentoReceita.retDetalhamentoReceita.Items[j].RetDetalhamentoReceita.descricao;
            end;
          end;
        end;

        if Assigned(FInfProduto) then
          FInfProduto.Free;

        FInfProduto := TRetProduto.Create;

        if SameText(retReceita.Items[i].RetInfReceita.exigeProduto, 'S') then
        begin
          InfProduto.Leitor.Arquivo := FLeitor.Grupo;
          InfProduto.LerXml;

          if InfProduto.retProduto.Count > 0 then
          begin
            for j := 0 to InfProduto.retProduto.Count - 1 do
            begin
              Produto := FretReceita.Items[i].FretProduto.New;
              Produto.RetProduto.codigo := InfProduto.retProduto.Items[j].RetProduto.codigo;
              Produto.RetProduto.descricao := InfProduto.retProduto.Items[j].RetProduto.descricao;
            end;
          end;
        end;

        if Assigned(FInfPeriodoApuracao) then
          FInfPeriodoApuracao.Free;

        FInfPeriodoApuracao := TRetPeriodoApuracao.Create;

        if SameText(retReceita.Items[i].RetInfReceita.exigePeriodoReferencia, 'S') then
        begin
          if SameText(retReceita.Items[i].RetInfReceita.exigePeriodoApuracao, 'S') then
          begin
            InfPeriodoApuracao.Leitor.Arquivo := FLeitor.Grupo;
            InfPeriodoApuracao.LerXml;

            if InfPeriodoApuracao.retPeriodoApuracao.Count > 0 then
            begin
              for j := 0 to InfPeriodoApuracao.retPeriodoApuracao.Count - 1 do
              begin
                PeriodoApuracao := FretReceita.Items[i].FretPeriodoApuracao.New;
                PeriodoApuracao.RetPeriodoApuracao.codigo :=
                  InfPeriodoApuracao.retPeriodoApuracao.Items[j].RetPeriodoApuracao.codigo;
                PeriodoApuracao.RetPeriodoApuracao.descricao :=
                  InfPeriodoApuracao.retPeriodoApuracao.Items[j].RetPeriodoApuracao.descricao;
              end;
            end;
          end;
        end;

        if Assigned(FInfTipoDocumentoOrigem) then
          FInfTipoDocumentoOrigem.Free;

        FInfTipoDocumentoOrigem := TRetTipoDocumentoOrigem.Create;

        if SameText(retReceita.Items[i].RetInfReceita.exigeDocumentoOrigem, 'S') then
        begin
          InfTipoDocumentoOrigem.Leitor.Arquivo := FLeitor.Grupo;
          InfTipoDocumentoOrigem.LerXml;

          if InfTipoDocumentoOrigem.retTipoDocumentoOrigem.Count > 0 then
          begin
            for j := 0 to InfTipoDocumentoOrigem.retTipoDocumentoOrigem.Count - 1 do
            begin
              TipoDocumentoOrigem := FretReceita.Items[i].FretTipoDocumentoOrigem.New;
              TipoDocumentoOrigem.RetTipoDocumentoOrigem.codigo :=
                InfTipoDocumentoOrigem.retTipoDocumentoOrigem.Items[j].RetTipoDocumentoOrigem.codigo;
              TipoDocumentoOrigem.RetTipoDocumentoOrigem.descricao :=
                InfTipoDocumentoOrigem.retTipoDocumentoOrigem.Items[j].RetTipoDocumentoOrigem.descricao;
            end;
          end;
        end;

        if Assigned(FInfCampoAdicional) then
          FInfCampoAdicional.Free;

        FInfCampoAdicional := TRetCampoAdicional.Create;

        if SameText(retReceita.Items[i].RetInfReceita.exigeCamposAdicionais, 'S') then
        begin
          InfCampoAdicional.Leitor.Arquivo := FLeitor.Grupo;
          InfCampoAdicional.LerXml;

          if InfCampoAdicional.retCampoAdicional.Count > 0 then
          begin
            for j := 0 to InfCampoAdicional.retCampoAdicional.Count - 1 do
            begin
              CampoAdicional := FretReceita.Items[i].FretCampoAdicional.New;
              CampoAdicional.RetCampoAdicional.obrigatorio :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.obrigatorio;
              CampoAdicional.RetCampoAdicional.codigo :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.codigo;
              CampoAdicional.RetCampoAdicional.tipo :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.tipo;
              CampoAdicional.RetCampoAdicional.tamanho :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.tamanho;
              CampoAdicional.RetCampoAdicional.casasDecimais :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.casasDecimais;
              CampoAdicional.RetCampoAdicional.titulo :=
                InfCampoAdicional.retCampoAdicional.Items[j].RetCampoAdicional.titulo;
            end;
          end;
        end;
        
        inc(i);
      end;

      if i = 0 then
        retReceita.New;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
