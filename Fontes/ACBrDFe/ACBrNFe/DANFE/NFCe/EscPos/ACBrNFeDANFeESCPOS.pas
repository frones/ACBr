{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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

unit ACBrNFeDANFeESCPOS;

interface

uses
  Classes, SysUtils, {$IFDEF FPC} LResources, {$ENDIF}
  ACBrBase, ACBrNFeDANFEClass, ACBrPosPrinter,
  pcnNFe, pcnEnvEventoNFe, pcnInutNFe;

const
  CLarguraRegiaoEsquerda = 270;

type
  { TACBrNFeDANFeESCPOS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFeDANFeESCPOS = class(TACBrNFeDANFCEClass)
  private
    FPosPrinter : TACBrPosPrinter ;
    FSuportaCondensado: Boolean;
    function TagLigaCondensado: String;
    function ColunasCondensado: Integer;
    function FazerImpressaoLateral: Boolean;

    procedure AjustaStringList(AStringList: TStringList);
    procedure MontarEnviarDANFE(NFE: TNFe; const AResumido: Boolean);
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
    procedure GerarIdentificacaoInutilizacao;
    procedure GerarDadosInutilizacao(fpInutNFe: TinutNFe);
  protected
    FpNFe: TNFe;
    FpEvento: TEventoNFe;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AtivarPosPrinter;

    procedure GerarCabecalho;
    procedure GerarIdentificacaodoDANFE;
    function GerarMensagemContingencia(CaracterDestaque : Char): String;
    procedure GerarDetalhesProdutosServicos;
    procedure GerarInformacoesTotais;
    procedure GerarPagamentos;
    procedure GerarInformacoesConsultaChaveAcesso;
    function GerarInformacoesConsumidor(Lateral: Boolean = False): String;
    function GerarInformacoesIdentificacaoNFCe(Lateral: Boolean = False): String;
    function GerarMensagemFiscal: String;
    function GerarInformacoesQRCode(const DadosQRCode: String; Cancelamento: Boolean = False): String;
    procedure GerarMensagemInteresseContribuinte;
    procedure GerarTotalTributos;

    procedure GerarRodape;
    procedure GerarDadosEvento;
    procedure GerarObservacoesEvento;

    function CalcularDadosQRCode: String;
    function ValidaLogoBmp(const APathLogo:string):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); override;
    procedure ImprimirDANFECancelado(NFE: TNFe = nil); override;
    procedure ImprimirEVENTO(NFE : TNFe = nil);override;
    procedure ImprimirINUTILIZACAO(ANFe: TNFe = nil); override;

    procedure ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo : Boolean = True);
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;
    property SuportaCondensado: Boolean read FSuportaCondensado write FSuportaCondensado default True;
  end;

implementation

uses
  strutils,
  Math,
  ACBrNFe,
  ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrConsts,
  ACBrDFeDANFeReport,
  pcnConversao,
  pcnAuxiliar,
  ACBrImage;

{ TACBrNFeDANFeESCPOS }

constructor TACBrNFeDANFeESCPOS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPosPrinter := Nil;
  FSuportaCondensado := True;
end;

destructor TACBrNFeDANFeESCPOS.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrNFeDANFeESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if AValue <> FPosPrinter then
  begin
     if Assigned(FPosPrinter) then
        FPosPrinter.RemoveFreeNotification(Self);

     FPosPrinter := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end;

function TACBrNFeDANFeESCPOS.TagLigaCondensado: String;
begin
  if FSuportaCondensado then
    Result := '<c>'
  else
    Result := '';
end;

function TACBrNFeDANFeESCPOS.FazerImpressaoLateral: Boolean;
begin
  Result := SuportaCondensado and
            (PosPrinter.Colunas >= 48) and
            (PosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0);
end;

procedure TACBrNFeDANFeESCPOS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
       FPosPrinter := nil ;
  end;
end;

procedure TACBrNFeDANFeESCPOS.AtivarPosPrinter;
begin
  if not Assigned( FPosPrinter ) then
    raise Exception.Create('Componente PosPrinter não associado');

  FPosPrinter.Ativar;
end;

procedure TACBrNFeDANFeESCPOS.GerarCabecalho;
var
  DadosCabecalho: TStringList;
  Altura: Integer;
  TextoLateral: String;
begin
  if FazerImpressaoLateral and ImprimeLogoLateral then
  begin
    TextoLateral := TagLigaCondensado;
    if (Trim(FpNFe.Emit.xFant) <> '') and ImprimeNomeFantasia then
       TextoLateral := TextoLateral +
                       QuebraLinhas('<n>' + FpNFe.Emit.xFant + ' </n>',
                        Trunc(ColunasCondensado/2));

    TextoLateral := TextoLateral +
                    QuebraLinhas('<n>' + FpNFe.Emit.xNome + '</n>'+
                    ' CNPJ:'+ FormatarCNPJ(FpNFe.Emit.CNPJCPF) +
                    ' IE:'+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF),
                      Trunc(ColunasCondensado/2))+sLineBreak;

    TextoLateral := TextoLateral +
                    QuebraLinhas(Trim(Trim(FpNFe.Emit.EnderEmit.xLgr) +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.nro)<>'',', '+Trim(FpNFe.Emit.EnderEmit.nro),'') + ' ' +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.xCpl)<>'',Trim(FpNFe.Emit.EnderEmit.xCpl) + ' ','') +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.xBairro)<>'',Trim(FpNFe.Emit.EnderEmit.xBairro) + ' ','') +
                    Trim(FpNFe.Emit.EnderEmit.xMun) + '-' + Trim(FpNFe.Emit.EnderEmit.UF) + ' ' +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.fone)<>'','<n>'+FormatarFone(Trim(FpNFe.Emit.EnderEmit.fone))+'</n>','')),
                      Trunc(ColunasCondensado/2));

    DadosCabecalho := TStringList.Create;
    try
      DadosCabecalho.Text := TextoLateral;
      Altura := max(FPosPrinter.CalcularAlturaTexto(DadosCabecalho.Count),250);
    finally
      DadosCabecalho.Free;
    end;
    FPosPrinter.Buffer.Add('</zera><mp>' +
                          FPosPrinter.ConfigurarRegiaoModoPagina(0,0,Altura,CLarguraRegiaoEsquerda));

    FPosPrinter.Buffer.Add(IfThen(ValidaLogoBmp(logo),'<bmp>'+Logo+'</bmp>','</lf></logo>'));

    FPosPrinter.Buffer.Add(FPosPrinter.ConfigurarRegiaoModoPagina(CLarguraRegiaoEsquerda,0,Altura,325) +
                          TextoLateral +
                          '</mp>');
  end
  else
  begin
    FPosPrinter.Buffer.Add('</zera></ce>'+ IfThen(ValidaLogoBmp(logo),'<bmp>'+Logo+'</bmp>','</lf></logo>'));

    if (Trim(FpNFe.Emit.xFant) <> '') and ImprimeNomeFantasia then
       FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado+'<n>' +  FpNFe.Emit.xFant + '</n>');

    FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado+
      QuebraLinhas( 'CNPJ: '+ FormatarCNPJ(FpNFe.Emit.CNPJCPF) + ' <n>' +
                    FpNFe.Emit.xNome + '</n>', ColunasCondensado) );

    FPosPrinter.Buffer.Add( TagLigaCondensado +
      QuebraLinhas(Trim(FpNFe.Emit.EnderEmit.xLgr) + ', ' +
        Trim(FpNFe.Emit.EnderEmit.nro) + ' ' +
        Trim(FpNFe.Emit.EnderEmit.xCpl) + ' ' +
        Trim(FpNFe.Emit.EnderEmit.xBairro) +  ' ' +
        Trim(FpNFe.Emit.EnderEmit.xMun) + '-' + Trim(FpNFe.Emit.EnderEmit.UF)+' '+
        FormatarCEP(FpNFe.Emit.EnderEmit.CEP), ColunasCondensado) );

    if not EstaVazio(FpNFe.Emit.EnderEmit.fone) then
      FPosPrinter.Buffer.Add('</ce></fn>'+TagLigaCondensado+
      QuebraLinhas('Fone:<n>'+ FormatarFone(FpNFe.Emit.EnderEmit.fone)+
                   '</n> I.E.:'+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF),
                   ColunasCondensado) )
    else
      FPosPrinter.Buffer.Add('</ce></fn>'+TagLigaCondensado+'I.E.: '+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF))
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarIdentificacaodoDANFE;
var
  MsgContingencia: String;
begin
  FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado+'<n>' +
    QuebraLinhas(ACBrStr('Documento Auxiliar da Nota Fiscal de Consumidor Eletrônica'),
    ColunasCondensado) + '</n>');

  MsgContingencia := GerarMensagemContingencia('=');
  if NaoEstaVazio(Trim(MsgContingencia)) then
    FPosPrinter.Buffer.Add(MsgContingencia);
end;

procedure TACBrNFeDANFeESCPOS.GerarDetalhesProdutosServicos;
var
  i: Integer;
  nTamDescricao: Integer;
  VlrAcrescimo, VlrLiquido: Double;
  sItem, sCodigo, sDescricao, sQuantidade, sUnidade, sVlrUnitario, sVlrProduto,
    LinhaCmd: String;
  sDescricaoAd: String;
  posQuebra, posDescricao: Integer;

const
  tagDescricao = '[DesProd]';
begin
  if ImprimeItens then
  begin
    FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado+
      ACBrStr(PadSpace('#|Código|Descrição|Qtde|Un|Valor unit.|Valor total',
      ColunasCondensado, '|')));

    for i := 0 to FpNFe.Det.Count - 1 do
    begin
      with FpNFe.Det.Items[i] do
      begin
        sItem           :=          IntToStrZero( Prod.nItem, 3);
        sCodigo         :=          ManterCodigo( Prod.cEAN , Prod.cProd );
        sDescricao      :=           ManterXProd( FpNFe, i );
        sQuantidade     :=    FormatarQuantidade( Prod.QCom, False );
        sUnidade        :=                  Trim( Prod.uCom);
        sVlrUnitario    := FormatarValorUnitario( Prod.VUnCom );
        sVlrProduto     :=         FormatFloatBr( Prod.vProd );

        sDescricaoAd    := '';

        if ImprimeEmUmaLinha then
        begin
          LinhaCmd := sItem + ' ' + sCodigo + ' ' + tagDescricao + ' ' + sQuantidade + ' ' +
            sUnidade + ' X ' + sVlrUnitario + ' ' + sVlrProduto;

          // prepara impressão da segunda linha da descrição (informação adicional)
          posQuebra := Pos(sLineBreak, sDescricao);
          posDescricao := Pos(tagDescricao, LinhaCmd);

          if posQuebra > 0 then
          begin
            sDescricaoAd := Copy(sDescricao, posQuebra + Length(sLineBreak), MaxInt);
            sDescricao := Copy(sDescricao, 1, posQuebra - 1);
          end;

          // acerta tamanho da descrição
          nTamDescricao := ColunasCondensado - Length(LinhaCmd) + Length(tagDescricao);
          sDescricao := PadRight(Copy(sDescricao, 1, nTamDescricao), nTamDescricao);

          LinhaCmd := StringReplace(LinhaCmd, tagDescricao, sDescricao, [rfReplaceAll]);
          if sDescricaoAd <> '' then
            LinhaCmd := LinhaCmd + sLineBreak + StringOfChar(' ', posDescricao - 1) + sDescricaoAd;
          FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);
        end
        else
        begin
          LinhaCmd := sItem + ' ' + sCodigo + ' ' + sDescricao;
          FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);

          if (ColunasCondensado >= 48) then
          begin
            LinhaCmd := PadRight(sQuantidade, 15) + ' ' + PadRight(sUnidade, 6) + ' X ' +
                        PadRight(sVlrUnitario, 13) + '|' + sVlrProduto;
          end
          else
            LinhaCmd := sQuantidade + '|' + sUnidade + ' X ' +
                        sVlrUnitario + '|' + sVlrProduto;

          LinhaCmd := padSpace(LinhaCmd, ColunasCondensado, '|');
          FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);
        end;

        if ImprimeDescAcrescItem then
        begin
          VlrAcrescimo := Prod.vSeg + Prod.vOutro;
          VlrLiquido   := (Prod.qCom * Prod.vUnCom) + (VlrAcrescimo + Prod.vFrete) - Prod.vDesc;

          // desconto
          if Prod.vDesc > 0 then
          begin
            LinhaCmd := '</ae>'+TagLigaCondensado + padSpace(
                'Desconto ' + padLeft(FormatFloatBr(Prod.vDesc, '-,0.00'), 15, ' ')
                +IIf((VlrAcrescimo+Prod.vFrete > 0),'','|' + FormatFloatBr(VlrLiquido)) ,
                ColunasCondensado, '|');
            FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);
          end;

          // Frete
          if Prod.vFrete > 0 then
          begin
            LinhaCmd := '</ae>'+TagLigaCondensado + padSpace(
                'Frete ' + padLeft(FormatFloatBr(Prod.vFrete, '+,0.00'), 15, ' ')
                +IIf((VlrAcrescimo > 0),'','|' + FormatFloatBr(VlrLiquido)) ,
                ColunasCondensado, '|');
            FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);
          end;

          // acrescimo
          if VlrAcrescimo > 0 then
          begin
            LinhaCmd := '</ae>'+TagLigaCondensado + ACBrStr(padSpace(
                'Acréscimo ' + padLeft(FormatFloatBr(VlrAcrescimo, '+,0.00'), 15, ' ')
                + '|' + FormatFloatBr(VlrLiquido),
                ColunasCondensado, '|'));
            FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado + LinhaCmd);
          end;
        end;

        if ExibeInforAdicProduto = infSeparadamente then
        begin
          LinhaCmd := FpNFe.Det[i].infAdProd;
          if Trim(LinhaCmd) <> '' then
            FPosPrinter.Buffer.Add(TagLigaCondensado + LinhaCmd);
        end;

      end;
    end;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarInformacoesTotais;
var
  SufixoTitulo, TagLigaExpandido, TagDesligaExpandido: String;
  FatorExp: Integer;
  ImprimeTotalNoFinal: Boolean;
begin
  if (ColunasCondensado >= 46) then
  begin
    TagLigaExpandido := '<e>';
    TagDesligaExpandido := '</e>';
    FatorExp := 2;
  end
  else
  begin
    TagLigaExpandido := '';
    TagDesligaExpandido := '';
    FatorExp := 1;
  end;

  if ImprimeDescAcrescItem then
    SufixoTitulo := ' total'
  else
    SufixoTitulo := '';

  ImprimeTotalNoFinal := (FpNFe.Total.ICMSTot.vDesc > 0) or
                         ((FpNFe.Total.ICMSTot.vOutro + FpNFe.Total.ICMSTot.vFrete + FpNFe.Total.ICMSTot.vSeg) > 0);

  FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace('Qtde. total de itens|' +
     IntToStrZero(FpNFe.Det.Count, 3), ColunasCondensado, '|'));

  if ImprimeTotalNoFinal then  // Se não for reimprimir o Total no Final, use Expandido
    FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace('Valor total R$|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vProd + FpNFe.Total.ISSQNtot.vServ),
       ColunasCondensado, '|'))
  else
    FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado+TagLigaExpandido +
       PadSpace('Valor total R$|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vProd + FpNFe.Total.ISSQNtot.vServ),
       ColunasCondensado div FatorExp, '|') + TagDesligaExpandido);

  if (FpNFe.Total.ICMSTot.vDesc > 0) then
    FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace('Desconto'+SufixoTitulo+'|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vDesc, '-,0.00'),
       ColunasCondensado, '|'));

  if (FpNFe.Total.ICMSTot.vOutro+FpNFe.Total.ICMSTot.vSeg) > 0 then
    FPosPrinter.Buffer.Add(TagLigaCondensado + ACBrStr(PadSpace('Acréscimo'+SufixoTitulo+'|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vOutro+FpNFe.Total.ICMSTot.vSeg, '+,0.00'),
       ColunasCondensado, '|')));

  if (FpNFe.Total.ICMSTot.vFrete) > 0 then
    FPosPrinter.Buffer.Add(TagLigaCondensado + ACBrStr(PadSpace('Frete'+SufixoTitulo+'|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vFrete, '+,0.00'),
       ColunasCondensado, '|')));

  if ImprimeTotalNoFinal then
    FPosPrinter.Buffer.Add('</ae>'+TagLigaCondensado+TagLigaExpandido +
       PadSpace('Valor a Pagar R$|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vNF),
       ColunasCondensado div FatorExp, '|') + TagDesligaExpandido);
end;

procedure TACBrNFeDANFeESCPOS.GerarPagamentos;
var
  i: Integer;
  Troco: Real;
begin
  //Total := 0;
  FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace('FORMA DE PAGAMENTO | VALOR PAGO R$',
     ColunasCondensado, '|'));

  for i := 0 to FpNFe.pag.Count - 1 do
  begin
    with FpNFe.pag.Items[i] do
    begin
      FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace(ManterDescricaoPagamentos(FpNFe.pag.Items[i]) + '|' +
         FormatFloatBr(vPag),
         ColunasCondensado, '|'));
    end;
  end;

  Troco := IIf(FpNFe.pag.vTroco > 0,FpNFe.pag.vTroco,vTroco);

  if Troco > 0 then
    FPosPrinter.Buffer.Add(TagLigaCondensado + PadSpace('Troco R$|' +
       FormatFloatBr(Troco), ColunasCondensado, '|'));

end;

procedure TACBrNFeDANFeESCPOS.GerarInformacoesConsultaChaveAcesso;
var
  UrlChave: String;
begin
  // chave de acesso
  FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado+'<n>Consulte pela Chave de Acesso em</n>');
  if EstaVazio(FpNFe.infNFeSupl.urlChave) then
    UrlChave := TACBrNFe(ACBrNFe).GetURLConsultaNFCe(FpNFe.ide.cUF, FpNFe.ide.tpAmb, FpNFe.infNFe.Versao)
  else
    UrlChave := FpNFe.infNFeSupl.urlChave;

  FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado + QuebraLinhas(UrlChave, ColunasCondensado) );
  FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado + QuebraLinhas(FormatarChaveAcesso(OnlyNumber(FpNFe.infNFe.ID)), ColunasCondensado) );
end;

procedure TACBrNFeDANFeESCPOS.GerarTotalTributos;
var
  MsgTributos : String;
begin
  if (ImprimeTributos = trbNenhum) then
    Exit;

  if (ImprimeTributos = trbSeparadamente) and ((vTribFed+vTribEst+vTribMun) > 0) then
  begin
     MsgTributos:= 'Tributos Incidentes Lei Federal 12.741/12 - Total R$ %s Federal R$ %s Estadual R$ %s Municipal R$ %s';
     FPosPrinter.Buffer.Add(TagLigaCondensado +
       QuebraLinhas(Format(MsgTributos,[FormatFloatBr(vTribFed + vTribEst + vTribMun),
                         FormatFloatBr(vTribFed),
                         FormatFloatBr(vTribEst),
                         FormatFloatBr(vTribMun)]), ColunasCondensado));
  end
  else
  begin
    if FpNFe.Total.ICMSTot.vTotTrib > 0 then
    begin
      MsgTributos:= 'Tributos Totais Incidentes(Lei Federal 12.741/12): R$ %s';
      FPosPrinter.Buffer.Add(TagLigaCondensado +
        QuebraLinhas(Format(MsgTributos,[FormatFloatBr(FpNFe.Total.ICMSTot.vTotTrib)]),
                          ColunasCondensado));
    end;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarMensagemInteresseContribuinte;
var
  TextoObservacao: string;
  i: Integer;
begin
  if ImprimeInfContr then
  begin
    for i := 0 to FpNFe.InfAdic.obsCont.Count - 1 do
    begin
      TextoObservacao := StringReplace(Trim(FpNFe.InfAdic.obsCont[i].xCampo) + ': ' +
          Trim(FpNFe.InfAdic.obsCont[i].xTexto), ';', sLineBreak, [rfReplaceAll]);
      FPosPrinter.Buffer.Add(TagLigaCondensado + TextoObservacao);
    end;
  end;

  TextoObservacao := Trim(FpNFe.InfAdic.infCpl);

  if TextoObservacao <> '' then
  begin
    TextoObservacao := StringReplace(FpNFe.InfAdic.infCpl, ';', sLineBreak, [rfReplaceAll]);
    FPosPrinter.Buffer.Add(TagLigaCondensado + TextoObservacao);
  end;
end;

function TACBrNFeDANFeESCPOS.GerarMensagemContingencia(CaracterDestaque: Char
  ): String;
var
  MsgContingencia: TStringList;
  Lateral: Boolean;
  AMsg: String;
begin
  Lateral := (CaracterDestaque=#0);
  MsgContingencia := TStringList.Create;
  try
    // se homologação imprimir o texto de homologação
    if (FpNFe.ide.tpAmb = taHomologacao) then
      MsgContingencia.Add('</ce>'+TagLigaCondensado+'<n>'+
        QuebraLinhas(ACBrStr('EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'),
                     Trunc(ColunasCondensado/ifthen(Lateral,2,1)))+
        '</n>');

    // se diferente de normal imprimir a emissão em contingência
    if (FpNFe.ide.tpEmis <> teNormal) and EstaVazio(FpNFe.procNFe.nProt) then
    begin
      AMsg := ACBrStr('EMITIDA EM CONTINGÊNCIA');

      if Lateral then
        MsgContingencia.Add('</ce></fn><n>'+QuebraLinhas(AMsg,
                    Trunc(FPosPrinter.ColunasFonteNormal/2))+'</n>')
      else
      begin
        if FPosPrinter.ColunasFonteNormal >= 46 then
          MsgContingencia.Add('</ce></fn><e><n>'+QuebraLinhas(AMsg,
                      FPosPrinter.ColunasFonteExpandida)+'</n></e>')
        else
          MsgContingencia.Add('</ce></fn><n>'+QuebraLinhas(AMsg,
                      FPosPrinter.ColunasFonteNormal)+'</n>');

      end;

      if CaracterDestaque <> #0 then
        MsgContingencia.Add(ACBrStr(TagLigaCondensado+'<n>'+
          PadCenter(' Pendente de autorização ',ColunasCondensado, CaracterDestaque)+'</n>'))
      else
        MsgContingencia.Add(ACBrStr('</ce>'+TagLigaCondensado+'<n>Pendente de autorização</n>'));
    end;
  finally
    Result := MsgContingencia.Text;
    MsgContingencia.Free;
  end;
end;

function TACBrNFeDANFeESCPOS.GerarInformacoesConsumidor(Lateral: Boolean
  ): String;
var
  LinhaCmd: String;
  DadosConsumidor: TStringList;
  Colunas: Integer;
begin
  Colunas := ColunasCondensado;
  if Lateral then
    Colunas := Trunc(Colunas/2);

  DadosConsumidor := TStringList.Create;
  try
    if (FpNFe.Dest.idEstrangeiro = '') and (FpNFe.Dest.CNPJCPF = '') then
    begin
      DadosConsumidor.Add(ACBrStr(TagLigaCondensado+'CONSUMIDOR NÃO IDENTIFICADO'));
    end
    else
    begin
      if FpNFe.Dest.idEstrangeiro <> '' then
        LinhaCmd := 'CONSUMIDOR - Id. Estrangeiro ' + FpNFe.Dest.idEstrangeiro
      else
      begin
        if Length(Trim(FpNFe.Dest.CNPJCPF)) > 11 then
          LinhaCmd := 'CONSUMIDOR - CNPJ '+ FormatarCNPJ(FpNFe.Dest.CNPJCPF)
        else
          LinhaCmd := 'CONSUMIDOR - CPF ' + FormatarCPF(FpNFe.Dest.CNPJCPF);
      end;

      DadosConsumidor.Add( '</ce>'+TagLigaCondensado+'<n>' +
                           QuebraLinhas(LinhaCmd, Colunas) +
                           '</n> ');
      DadosConsumidor.Add( QuebraLinhas(Trim(FpNFe.Dest.xNome), Colunas) );

      LinhaCmd := Trim(
        Trim(FpNFe.Dest.EnderDest.xLgr) + ' ' +
        IfThen(Trim(FpNFe.Dest.EnderDest.xLgr) = '','',Trim(FpNFe.Dest.EnderDest.nro)) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xCpl) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xBairro) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xMun) + ' ' +
        Trim(FpNFe.Dest.EnderDest.UF)
      );

      if LinhaCmd <> '' then
        DadosConsumidor.Add(TagLigaCondensado + QuebraLinhas(LinhaCmd, Colunas));
    end;
  finally
    Result := DadosConsumidor.Text;
    DadosConsumidor.Free;
  end;
end;

function TACBrNFeDANFeESCPOS.GerarInformacoesIdentificacaoNFCe(Lateral: Boolean
  ): String;
var
  InfoNFCe, InfoAut: String;
  Colunas: Integer;

  function ReplaceSoftBreak( ALine: String): String;
  begin
    Result := StringReplace( ALine, '|', ifthen(Lateral,sLineBreak,' '), [rfReplaceAll]);
  end;

begin
  Colunas := ColunasCondensado;
  if Lateral then
    Colunas := Trunc(Colunas/2);

  Result := '</ce>'+TagLigaCondensado+'<n>';
  InfoNFCe := ACBrStr('NFC-e nº ') + IntToStrZero(FpNFe.Ide.nNF, 9) +
              ACBrStr(' Série ') + IntToStrZero(FpNFe.Ide.serie, 3) + '|' +
              DateTimeToStr(FpNFe.ide.dEmi) + '</n>';

  if EstaVazio(Trim(FpNFe.procNFe.nProt)) then
    InfoNFCe := InfoNFCe + IfThen(ViaConsumidor, '|Via Consumidor', '|Via Empresa');

  // dados da nota eletronica de consumidor
  Result := Result +
            QuebraLinhas( ReplaceSoftBreak(InfoNFCe), Colunas) + sLineBreak;

  // protocolo de autorização
  if (FpNFe.Ide.tpEmis <> teOffLine) or NaoEstaVazio(FpNFe.procNFe.nProt) then
  begin
    InfoAut := TagLigaCondensado+'<n>'+ACBrStr('Protocolo de Autorização:')+'</n>|'+
                Trim(FpNFe.procNFe.nProt);
    Result := Result + QuebraLinhas( ReplaceSoftBreak(InfoAut), Colunas) + sLineBreak;

    if (FpNFe.procNFe.dhRecbto <> 0) then
    begin
      InfoAut := TagLigaCondensado+'<n>'+ACBrStr('Data de Autorização')+'</n>|'+
                 DateTimeToStr(FpNFe.procNFe.dhRecbto);
      Result := Result + QuebraLinhas( ReplaceSoftBreak(InfoAut), Colunas) + sLineBreak;
    end;
  end;

  Result := Result + '</fn>';
end;

function TACBrNFeDANFeESCPOS.GerarMensagemFiscal: String;
Var
  TextoObservacao: String;
  MensagemFiscal: TStringList;
begin
  MensagemFiscal := TStringList.Create;

  try
    TextoObservacao := Trim(FpNFe.InfAdic.infAdFisco);
    if TextoObservacao <> '' then
      MensagemFiscal.Add(TagLigaCondensado + StringReplace(TextoObservacao, ';', sLineBreak, [rfReplaceAll]));

    TextoObservacao := Trim(FpNFe.procNFe.xMsg);
    if TextoObservacao <> '' then
      MensagemFiscal.Add(TagLigaCondensado + TextoObservacao);
  finally
    Result := MensagemFiscal.Text;
    MensagemFiscal.Free;
  end;
end;

function TACBrNFeDANFeESCPOS.GerarInformacoesQRCode(const DadosQRCode: String;
  Cancelamento: Boolean): String;
var
  InfoQrCode: TStringList;
begin
  InfoQrCode := TStringList.Create;
  try
    if Cancelamento then
    begin
      InfoQrCode.Add('</fn></linha_simples>');
      InfoQrCode.Add('</ce>Consulta via leitor de QR Code');
    end;

    InfoQrCode.Add( '<qrcode_tipo>2</qrcode_tipo>'+
                    '<qrcode_error>0</qrcode_error>'+
                    '<qrcode_largura>'+IntToStr(FPosPrinter.ConfigQRCode.LarguraModulo)+'</qrcode_largura>'+
                    '<qrcode>'+DadosQRCode+'</qrcode>');

    if Cancelamento then
    begin
      InfoQrCode.Add(ACBrStr(TagLigaCondensado+'Protocolo de Autorização'));
      InfoQrCode.Add(TagLigaCondensado + Trim(FpNFe.procNFe.nProt) + ' ' +
         IfThen(FpNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FpNFe.procNFe.dhRecbto),
                '') + '</fn>');
      InfoQrCode.Add('</linha_simples>');
    end;
  finally
    Result := InfoQrCode.Text;
    InfoQrCode.Free;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarRodape;
begin
  // sistema
  if Sistema <> '' then
    FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado + QuebraLinhas(Sistema, ColunasCondensado));

  if Site <> '' then
    FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado + QuebraLinhas(Site, ColunasCondensado));

  // pular linhas e cortar o papel
  if FPosPrinter.CortaPapel then
    FPosPrinter.Buffer.Add('</corte>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>')
end;

procedure TACBrNFeDANFeESCPOS.MontarEnviarDANFE(NFE: TNFe;
  const AResumido: Boolean);
var
  AlturaMax, AlturaQRCode, EsquerdaQRCode: Integer;
  TextoLateral: TStringList;
  MsgContingencia, MsgFiscal, DadosQRCode: String;
begin
  if NFE = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create(ACBrStr('Componente ACBrNFe não atribuído'));

    FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFE;
  end
  else
    FpNFe := NFE;

  GerarCabecalho;
  GerarIdentificacaodoDANFE;
  if not AResumido then
    GerarDetalhesProdutosServicos;

  GerarInformacoesTotais;
  GerarPagamentos;
  GerarInformacoesConsultaChaveAcesso;

  DadosQRCode := CalcularDadosQRCode;

  if FazerImpressaoLateral and ImprimeQRCodeLateral then
  begin
    FPosPrinter.Buffer.Add(' ');

    TextoLateral := TStringList.Create;
    try
      TextoLateral.Text := sLineBreak +
                           GerarInformacoesConsumidor(True) +
                           GerarInformacoesIdentificacaoNFCe(True) +
                           GerarMensagemFiscal +
                           GerarMensagemContingencia(#0);

      AjustaStringList(TextoLateral); // Ajusta corretamente o numero de Linhas

      AlturaQRCode := FPosPrinter.CalcularAlturaQRCodeAlfaNumM(DadosQRCode);
      AlturaMax := max( FPosPrinter.CalcularAlturaTexto(TextoLateral.Count), AlturaQRCode );
      EsquerdaQRCode := Trunc(max(CLarguraRegiaoEsquerda - Trunc(AlturaQRCode/2),0) / 2);

      FPosPrinter.Buffer.Add( '<mp>' +
                              FPosPrinter.ConfigurarRegiaoModoPagina(
                                EsquerdaQRCode, 0, AlturaMax,
                                (CLarguraRegiaoEsquerda-EsquerdaQRCode) ) +
                              GerarInformacoesQRCode(DadosQRCode, False));
      FPosPrinter.Buffer.Add( FPosPrinter.ConfigurarRegiaoModoPagina(
                                CLarguraRegiaoEsquerda, 0, AlturaMax, 325) +
                              TextoLateral.Text + '</mp>');
    finally
      TextoLateral.Free;
    end;
  end
  else
  begin
    FPosPrinter.Buffer.Add(GerarInformacoesConsumidor(False));
    FPosPrinter.Buffer.Add(GerarInformacoesIdentificacaoNFCe);

    MsgFiscal := GerarMensagemFiscal;
    if NaoEstaVazio(Trim(MsgFiscal)) then
      FPosPrinter.Buffer.Add(MsgFiscal);

    MsgContingencia := GerarMensagemContingencia(' ');
    if NaoEstaVazio(Trim(MsgContingencia)) then
      FPosPrinter.Buffer.Add(MsgContingencia);

    FPosPrinter.Buffer.Add(GerarInformacoesQRCode(DadosQRCode, False));
  end;

  GerarMensagemInteresseContribuinte;
  GerarTotalTributos;
  GerarRodape;

  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrNFeDANFeESCPOS.AjustaStringList(AStringList: TStringList);
var
  Linhas: String;
begin
  Linhas := AStringList.Text;
  AStringList.Clear;
  AStringList.Text := Linhas;
end;

procedure TACBrNFeDANFeESCPOS.ImprimirDANFE(NFE: TNFe);
begin
  IF Cancelada then
    ImprimirDANFECancelado(NFE)
  else
  begin
    AtivarPosPrinter;
    MontarEnviarDANFE(NFE, False);
  end;
end;

procedure TACBrNFeDANFeESCPOS.ImprimirDANFEResumido(NFE: TNFe);
var
  OldImprimirItens: Boolean;
begin
  AtivarPosPrinter;
  OldImprimirItens := ImprimeItens;
  try
    ImprimeItens := False;
    MontarEnviarDANFE(NFE, True);
  finally
    ImprimeItens := OldImprimirItens;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarDadosEvento;
const
  TAMCOLDESCR = 11;
begin
  if FpEvento.Evento.Count < 1 then
    Exit;

  // dados da nota eletrônica
  FPosPrinter.Buffer.Add('</fn></ce><n>Nota Fiscal para Consumidor Final</n>');
  FPosPrinter.Buffer.Add(ACBrStr('Número: ' + IntToStrZero(FpNFe.ide.nNF, 9) +
                                 ' Série: ' + IntToStrZero(FpNFe.ide.serie, 3)));
  FPosPrinter.Buffer.Add(ACBrStr('Emissão: ' + DateTimeToStr(FpNFe.ide.dEmi)) + '</n>');

  if FpNFe.Total.ICMSTot.vNF > 0 then
    FPosPrinter.Buffer.Add(ACBrStr('Valor Documento: R$ ' +  FormatFloatBr(FpNFe.Total.ICMSTot.vNF)) + '</n>');

  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add(TagLigaCondensado+'CHAVE ACESSO');
  FPosPrinter.Buffer.Add(FormatarChaveAcesso(OnlyNumber(FpNFe.infNFe.ID)));
  FPosPrinter.Buffer.Add('</linha_simples>');

  // dados do evento
  FPosPrinter.Buffer.Add('</fn><n>EVENTO</n>');
  FPosPrinter.Buffer.Add('</fn></ae>' + PadRight('Evento:', TAMCOLDESCR) +
     FpEvento.Evento[0].InfEvento.TipoEvento );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Descrição:', TAMCOLDESCR)) +
     FpEvento.Evento[0].InfEvento.DescEvento);
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Orgão:', TAMCOLDESCR)) +
     IntToStr(FpEvento.Evento[0].InfEvento.cOrgao) );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Ambiente:', TAMCOLDESCR) +
     IfThen(FpEvento.Evento[0].RetInfEvento.tpAmb = taProducao,
            'PRODUCAO', 'HOMOLOGAÇÃO - SEM VALOR FISCAL') ));
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Emissão:', TAMCOLDESCR)) +
     DateTimeToStr(FpEvento.Evento[0].InfEvento.dhEvento) );
  FPosPrinter.Buffer.Add( PadRight('Sequencia:', TAMCOLDESCR) +
     IntToStr(FpEvento.Evento[0].InfEvento.nSeqEvento) );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Versão:', TAMCOLDESCR)) +
     FpEvento.Evento[0].InfEvento.versaoEvento );
  FPosPrinter.Buffer.Add( PadRight('Status:', TAMCOLDESCR) +
     FpEvento.Evento[0].RetInfEvento.xMotivo );
  FPosPrinter.Buffer.Add( PadRight('Protocolo:', TAMCOLDESCR) +
     FpEvento.Evento[0].RetInfEvento.nProt );
  FPosPrinter.Buffer.Add( PadRight('Registro:', TAMCOLDESCR) +
     DateTimeToStr(FpEvento.Evento[0].RetInfEvento.dhRegEvento) );

  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrNFeDANFeESCPOS.GerarObservacoesEvento;
begin
  if FpEvento.Evento.Count < 1 then
    Exit;

  if FpEvento.Evento[0].InfEvento.detEvento.xJust <> '' then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    FPosPrinter.Buffer.Add('</fn></ce><n>JUSTIFICATIVA</n>');
    FPosPrinter.Buffer.Add('</fn></ae>' +
       FpEvento.Evento[0].InfEvento.detEvento.xJust );
  end
  else if FpEvento.Evento[0].InfEvento.detEvento.xCorrecao <> '' then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    FPosPrinter.Buffer.Add('</fn></ce><n>' + ACBrStr('CORREÇÃO') + '</n>' );
    FPosPrinter.Buffer.Add('</fn></ae>' +
       FpEvento.Evento[0].InfEvento.detEvento.xCorrecao );
  end;
end;

function TACBrNFeDANFeESCPOS.CalcularDadosQRCode: String;
begin
  if EstaVazio(Trim(FpNFe.infNFeSupl.qrCode)) then
    Result := TACBrNFe(ACBrNFe).GetURLQRCode(
      FpNFe.ide.cUF,
      FpNFe.ide.tpAmb,
      FpNFe.infNFe.ID,
      IfThen(FpNFe.Dest.idEstrangeiro <> '', FpNFe.Dest.idEstrangeiro, FpNFe.Dest.CNPJCPF),
      FpNFe.ide.dEmi,
      FpNFe.Total.ICMSTot.vNF,
      FpNFe.Total.ICMSTot.vICMS,
      FpNFe.signature.DigestValue,
      FpNFe.infNfe.Versao)
  else
    Result := FpNFe.infNFeSupl.qrCode;
end;

function TACBrNFeDANFeESCPOS.ValidaLogoBmp(const APathLogo: string): Boolean;
var
  LFileStream : TFileStream;
begin
   Result := False;

   if not FileExists(APathLogo) then
     Exit;

   LFileStream := TFileStream.Create(APathLogo, fmOpenRead or fmShareDenyWrite);
   try
     if not IsBMP(LFileStream, True) then
       Exit;

     //if LFileStream.Size <= 5000 then // Menor ou igual a 5 KB -- se necessário validar, ficar para futuro
       Result := True;

   finally
     LFileStream.Free;
   end;
end;

function TACBrNFeDANFeESCPOS.ColunasCondensado: Integer;
begin
  if FSuportaCondensado then
    Result := FPosPrinter.ColunasFonteCondensada
  else
    Result := FPosPrinter.ColunasFonteNormal;
end;

procedure TACBrNFeDANFeESCPOS.ImprimirDANFECancelado(NFE: TNFe);
begin
  if NFE = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create(ACBrStr('Componente ACBrNFe não atribuído'));

    if TACBrNFe(ACBrNFe).NotasFiscais.Count <= 0 then
      raise Exception.Create(ACBrStr('XML da NFe não informado, obrigatório para o modelo ESCPOS'))
    else
      FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFE;
  end
  else
    FpNFe := NFE;

  FpEvento := TACBrNFe(ACBrNFe).EventoNFe;
  if not Assigned(FpEvento) then
    raise Exception.Create('Arquivo de Evento não informado!');

  AtivarPosPrinter;
  GerarCabecalho;
  GerarDadosEvento;
  FPosPrinter.Buffer.Add(GerarInformacoesConsumidor);
  GerarObservacoesEvento;
  FPosPrinter.Buffer.Add(GerarInformacoesQRCode(CalcularDadosQRCode, True));
  GerarRodape;

  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrNFeDANFeESCPOS.ImprimirEVENTO(NFE: TNFe);
begin
  ImprimirDANFECancelado(NFE);
end;

procedure TACBrNFeDANFeESCPOS.ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo : Boolean = True);
var
  LinhaCmd: String;
begin
  LinhaCmd := '</zera>';
  if ALogo then
    LinhaCmd := LinhaCmd + '</ce></logo>';

  LinhaCmd := LinhaCmd + '</ae>';
  FPosPrinter.Buffer.Add(LinhaCmd);

  FPosPrinter.Buffer.AddStrings( ATexto );
  if ACortaPapel then
    FPosPrinter.Buffer.Add('</corte>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>');

  FPosPrinter.Imprimir('', True, True, True, AVias);
end;

procedure TACBrNFeDANFeESCPOS.GerarDadosInutilizacao(fpInutNFe: TinutNFe);
const
  iTamanhoColunaRotulos = 18;
var
  sTipoAmbiente: string;
begin
  FPosPrinter.Buffer.Add(ACBrStr('</fn></ce><n>INUTILIZAÇÃO</n>'));
  FPosPrinter.Buffer.Add('</fn></ae>' + PadRight('ÓRGÃO:', iTamanhoColunaRotulos) + IntToStr(fpInutNFe.RetInutNFe.cUF));
  case fpInutNFe.RetInutNFe.tpAmb of
    taProducao:
      sTipoAmbiente := ACBrStr('PRODUÇÃO');
    taHomologacao:
      sTipoAmbiente := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
  end;
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('AMBIENTE:', iTamanhoColunaRotulos)) + sTipoAmbiente);
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('ANO:', iTamanhoColunaRotulos)) + IntToStr(fpInutNFe.RetInutNFe.ano));
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('MODELO:', iTamanhoColunaRotulos)) + IntToStr(fpInutNFe.RetInutNFe.modelo));
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('SERIE:', iTamanhoColunaRotulos)) + IntToStr(fpInutNFe.RetInutNFe.serie));
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('NUM. INUTILIZADA:', iTamanhoColunaRotulos)) + IntToStr(fpInutNFe.RetInutNFe.nNFIni) + ACBrStr(' a ') + IntToStr(fpInutNFe.RetInutNFe.nNFFin));
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('STATUS:', iTamanhoColunaRotulos)) + IntToStr(fpInutNFe.RetInutNFe.cStat) + ' - ' + fpInutNFe.RetInutNFe.xMotivo);
  FPosPrinter.Buffer.Add(ACBrStr(PadRight('NÚM. PROTOCOLO:', iTamanhoColunaRotulos)) + fpInutNFe.RetInutNFe.nProt + ' ' + FormatDateTimeBr(fpInutNFe.RetInutNFe.dhRecbto));
  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrNFeDANFeESCPOS.GerarIdentificacaoInutilizacao;
begin
  FPosPrinter.Buffer.Add('</ce>'+TagLigaCondensado+'<n>' +
    QuebraLinhas(ACBrStr('INUTILIZAÇÃO DE NUMERAÇÃO DA NF-E'), ColunasCondensado) + '</n>');
  FPosPrinter.Buffer.Add('</ae> Não possui valor fiscal, simples representação do fato indicado abaixo.');
  FPosPrinter.Buffer.Add('CONSULTE A AUTENTICIDADE NO SITE DA SEFAZ AUTORIZADORA');
  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrNFeDANFeESCPOS.ImprimirINUTILIZACAO(ANFe: TNFe);
var
  fpInutNFe: TInutNFe;
  sJustificativa: string;

begin
  if ANFE = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create(ACBrStr('Componente ACBrNFe não atribuído'));

    if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
      FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFE;
  end
  else
    FpNFe := ANFE;

  fpInutNFe := TACBrNFe(ACBrNFe).InutNFe;
  if not Assigned(fpInutNFe) then
  begin
    raise Exception.Create(ACBrStr('Objeto de inutilização não atribuído.'));
  end;

  // Salvar TACBrNFe(ACBrNFe).InutNFe, ANFe
  GerarIdentificacaoInutilizacao;
  if Assigned(FpNFe) then
  begin
    GerarCabecalho;
    FPosPrinter.Buffer.Add('</linha_simples>');
  end;
  GerarDadosInutilizacao(fpInutNFe);

  sJustificativa := ACBrStr(fpInutNFe.RetInutNFe.xJust);
  if sJustificativa <> '' then
  begin
    FPosPrinter.Buffer.Add('</fn></ce><n>JUSTIFICATIVA</n>');
    FPosPrinter.Buffer.Add('</fn></ae>' + sJustificativa );
    FPosPrinter.Buffer.Add('</linha_simples>');
  end;
  GerarRodape;
  FPosPrinter.Imprimir('', False, True, True, NumCopias);
end;

{$IfDef FPC}
initialization
{$I ACBrNFeDANFeESCPOS.lrs}
{$EndIf}

end.
