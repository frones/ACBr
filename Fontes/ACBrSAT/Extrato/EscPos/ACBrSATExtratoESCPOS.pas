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

unit ACBrSATExtratoESCPOS;

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  ACBrBase, ACBrSATExtratoClass, ACBrPosPrinter,
  pcnCFe, pcnCFeCanc, pcnConversao;

const
  CLarguraRegiaoEsquerda = 290;

type
  TAutoSimNao = (rAuto, rSim, rNao);

  { TACBrSATExtratoESCPOS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoESCPOS = class( TACBrSATExtratoClass )
  private
    FAjusteDeAlturaLogoLateral: Boolean;
    FImprimeChaveEmUmaLinha: TAutoSimNao;
    FPosPrinter : TACBrPosPrinter ;

    procedure ImprimirCopias ;
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AtivarPosPrinter;

    procedure GerarCabecalho(Cancelamento: Boolean = False);
    procedure GerarItens;
    procedure GerarTotais(Cancelamento: Boolean = False);
    procedure GerarPagamentos;
    procedure GerarObsFisco;
    procedure GerarDadosEntrega;
    procedure GerarObsContribuinte(Resumido : Boolean = False );
    procedure GerarRodape(Cancelamento: Boolean = False);
    procedure GerarDadosCancelamento;
    procedure GerarFechamento;

    function SuportaQRCodeLateral: Boolean;
    function SuportaLogoLateral: Boolean;
    function ComandosQRCode(const DadosQRCode: String): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirExtrato(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;

    procedure ImprimirExtrato(AStream: TStream; ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(AStream: TStream; ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(AStream: TStream; ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;

    function GerarImpressaoFiscalMFe(ACFe : TCFe = nil) : String;
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;

    property ImprimeChaveEmUmaLinha: TAutoSimNao read FImprimeChaveEmUmaLinha write FImprimeChaveEmUmaLinha default rAuto;
    property AjusteDeAlturaLogoLateral: Boolean read FAjusteDeAlturaLogoLateral write FAjusteDeAlturaLogoLateral default False;
  end ;

implementation

uses
  strutils, math,
  ACBrValidador, ACBrUtil.Strings, 
  ACBrUtil.Math, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.DateTime, ACBrUtil.XMLHTML,
  ACBrConsts, ACBrDFeUtil, ACBrSATClass;

{$IFNDEF FPC}
   {$R ACBrSATExtratoESCPOS.dcr}
{$ENDIF}

{ TACBrSATExtratoESCPOS }

constructor TACBrSATExtratoESCPOS.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FPosPrinter := Nil;

  FImprimeChaveEmUmaLinha := rAuto;
  FAjusteDeAlturaLogoLateral := False;
end;

destructor TACBrSATExtratoESCPOS.Destroy;
begin
  inherited;
end;

procedure TACBrSATExtratoESCPOS.GerarCabecalho(Cancelamento: Boolean);
var
  sCFe, DocsEmit, TextoLateral: String;
  MetadeColunas, Altura, LinhasTextoLateral: Integer;
  SL: TStringList;
  ImprimiuConsumidor: Boolean;

  function LinhaEnderecoEmissor: String;
  begin
    Result := Trim(CFe.Emit.EnderEmit.xLgr)+
              ifthen(Trim(CFe.Emit.EnderEmit.nro)<>'',', '+Trim(CFe.Emit.EnderEmit.nro),'') + ' ' +
              ifthen(Trim(CFe.Emit.EnderEmit.xCpl)<>'',Trim(CFe.Emit.EnderEmit.xCpl) + ' ','') +
              ifthen(Trim(CFe.Emit.EnderEmit.xBairro)<>'',Trim(CFe.Emit.EnderEmit.xBairro) + ' ','') +
              Trim(CFe.Emit.EnderEmit.xMun)+'-'+CUFtoUF(CFe.ide.cUF)+' '+
              FormatarCEP(CFe.Emit.EnderEmit.CEP);
  end;

begin
  if SuportaLogoLateral then
  begin
    MetadeColunas := Trunc(FPosPrinter.ColunasFonteCondensada/2)-2;
    TextoLateral := '';
    if (Trim(CFe.Emit.xFant) <> '') then
      TextoLateral := TextoLateral + CFe.Emit.xFant + sLineBreak;

    TextoLateral := TextoLateral +
                    CFe.Emit.xNome + sLineBreak +
                    LinhaEnderecoEmissor;

    TextoLateral := QuebraLinhas(TextoLateral, MetadeColunas);

    if (Trim(CFe.Emit.xFant) <> '') then
      TextoLateral := AplicarAtributoTexto(TextoLateral, CFe.Emit.xFant, '<n>')
    else
      TextoLateral := AplicarAtributoTexto(TextoLateral, CFe.Emit.xNome, '<n>');

    SL := TStringList.Create;
    try
      SL.Text := TextoLateral;
      LinhasTextoLateral := SL.Count;
      Altura := max(FPosPrinter.CalcularAlturaTexto(LinhasTextoLateral),230);
    finally
      SL.Free;
    end;

    FPosPrinter.Buffer.Add('</zera><mp>' +
                           FPosPrinter.ConfigurarRegiaoModoPagina(0,0,Altura,CLarguraRegiaoEsquerda)+
                           IfThen(AjusteDeAlturaLogoLateral, StringOfChar(LF, max(LinhasTextoLateral-1,0)), '')+'</logo>');
    FPosPrinter.Buffer.Add(FPosPrinter.ConfigurarRegiaoModoPagina(CLarguraRegiaoEsquerda,0,Altura,325) +
                           '</ae><c>'+TextoLateral +
                           '</mp>');
  end
  else
  begin
    FPosPrinter.Buffer.Add('</zera></ce></logo>');
    FPosPrinter.Buffer.Add('<n>'+CFe.Emit.xFant+'</n>');

    FPosPrinter.Buffer.Add('<c>'+CFe.Emit.xNome);
    FPosPrinter.Buffer.Add(QuebraLinhas( LinhaEnderecoEmissor, FPosPrinter.ColunasFonteCondensada));
  end;

  DocsEmit := 'CNPJ:'+FormatarCNPJ(CFe.Emit.CNPJ) + ' IE:'+Trim(CFe.Emit.IE);

  { Verifica se existe valor no campo IM }
  if (CFe.Emit.IM <> '') and (CFe.Emit.IM <> '0') then
    DocsEmit := DocsEmit + ' IM:'+Trim(CFe.Emit.IM);

  FPosPrinter.Buffer.Add( '</ce><c>'+ DocsEmit);

  if (CFe.ide.tpAmb = taHomologacao) then
    sCFe := StringOfChar('0',6)
  else if Cancelamento then
    sCFe := IntToStrZero( CFeCanc.ide.nCFe, 6)
  else
    sCFe := IntToStrZero( CFE.ide.nCFe, 6);

  FPosPrinter.Buffer.Add('</ce><c><n>EXTRATO No. <e>' + sCFe + '</e>'+ ACBrStr(' do CUPOM FISCAL ELETRÔNICO - SAT</n>'));
  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add('</ce></fn><n>CANCELAMENTO</n>');
    FPosPrinter.Buffer.Add(ACBrStr('<c><n>DADOS DO CUPOM FISCAL ELETRÔNICO CANCELADO</n>'));
  end;

  if (CFe.ide.tpAmb = taHomologacao) then
  begin
    FPosPrinter.Buffer.Add('</ce></fn>');
    FPosPrinter.Buffer.Add(' = T E S T E =');
    FPosPrinter.Buffer.Add(' ');
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FPosPrinter.Buffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FPosPrinter.Buffer.Add(' ');
  end;

  if not SuportaQRCodeLateral then  // Se não suporta o QRCode Lateral, deve imprimir consumidor no cabeçalho (como antes)
  begin
    ImprimiuConsumidor := False;
    if ImprimeCPFNaoInformado or (Trim(Cfe.Dest.CNPJCPF) <> '') then
    begin
      ImprimiuConsumidor := True;
      FPosPrinter.Buffer.Add('</ae><c></linha_simples>');
      FPosPrinter.Buffer.Add('CPF/CNPJ do Consumidor: '+
                  IfThen( Trim(CFe.Dest.CNPJCPF)<>'',
                          FormatarCNPJouCPF(CFe.Dest.CNPJCPF),
                          ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')));
    end;

    if (Trim(CFe.Dest.xNome) <> '') then
    begin
      ImprimiuConsumidor := True;
      FPosPrinter.Buffer.Add( ACBrStr('Razão Social/Nome: ')+CFe.Dest.xNome );
    end;

    if ImprimiuConsumidor and (not Cancelamento) then
      FPosPrinter.Buffer.Add('</linha_simples>');
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarItens;
var
  i: Integer;
  nTamDescricao: Integer;
  fQuant: Double;
  sItem, sCodigo, sDescricao, sQuantidade, sUnidade, sVlrUnitario, sVlrBruto,
    sVlrImpostos, LinhaCmd: String;
  GapItem: Boolean;
begin
  FPosPrinter.Buffer.Add('</ae><c>'+
                         PadSpace('#|COD|DESC|QTD|UN|VL UN R$|(VL TR R$)*|VL ITEM R$',
                         FPosPrinter.ColunasFonteCondensada,'|'));
  FPosPrinter.Buffer.Add('</linha_simples>');

  for i := 0 to CFe.Det.Count - 1 do
  begin
    sItem := IntToStrZero(CFe.Det.Items[i].nItem, 3);
    if ImprimeCodigoEan and (Trim(CFe.Det.Items[i].Prod.cEAN) <> '') then
      sCodigo := Trim(CFe.Det.Items[i].Prod.cEAN)
    else
      sCodigo := Trim(CFe.Det.Items[i].Prod.cProd);

    sDescricao := Trim(CFe.Det.Items[i].Prod.xProd);

    // formatar conforme configurado somente quando houver decimais
    // caso contrário mostrar somente o número inteiro
    fQuant := CFe.Det.Items[i].Prod.QCom;
    if (Frac(fQuant) > 0) then  // Tem decimais ?
      sQuantidade := FormatFloatBr(fQuant, CasasDecimais.MaskqCom )
    else
      sQuantidade := IntToStr(Trunc(fQuant));

    sUnidade := Trim(CFe.Det.Items[i].Prod.uCom);

    // formatar conforme configurado
    sVlrUnitario := FormatFloatBr(CFe.Det.Items[i].Prod.vUnCom,
      IfThen(CFe.Det.Items[i].Prod.EhCombustivel, ',0.000', CasasDecimais.MaskvUnCom));

    if (CFe.Det.Items[i].Imposto.vItem12741 > 0) then
      sVlrImpostos := ' ('+FormatFloatBr(CFe.Det.Items[i].Imposto.vItem12741)+') '
    else
      sVlrImpostos := ' ';

    sVlrBruto := FormatFloatBr(CFe.Det.Items[i].Prod.vProd);

    if ImprimeEmUmaLinha then
    begin
      LinhaCmd := sItem + ' ' + sCodigo + ' ' + '[DesProd] ' + sQuantidade + ' ' +
        sUnidade + ' X ' + sVlrUnitario + sVlrImpostos + sVlrBruto;

      // acerta tamanho da descrição
      nTamDescricao := FPosPrinter.ColunasFonteCondensada - Length(LinhaCmd) + 9;
      sDescricao := PadRight(Copy(sDescricao, 1, nTamDescricao), nTamDescricao);

      LinhaCmd := StringReplace(LinhaCmd, '[DesProd]', sDescricao, [rfReplaceAll]);
      FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
    end
    else
    begin
      LinhaCmd := sItem + ' ' + sCodigo + ' ' + sDescricao;
      FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);

      if (FPosPrinter.ColunasFonteCondensada >= 48) then
        LinhaCmd := PadRight(sQuantidade, 15) + ' ' + PadRight(sUnidade, 6) + ' X ' +
                    PadRight(sVlrUnitario, 13) + '|' + sVlrImpostos + sVlrBruto
      else
        LinhaCmd := sQuantidade  + '|' + sUnidade + ' X ' +
                    sVlrUnitario + '|' + sVlrImpostos + sVlrBruto;

      LinhaCmd := padSpace(LinhaCmd, FPosPrinter.ColunasFonteCondensada, '|');
      FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
    end;

    GapItem := False;

    if ImprimeDescAcrescItem then
    begin
      // desconto
      if (CFe.Det.Items[i].Prod.vDesc > 0) then
      begin
        GapItem := True;
        FPosPrinter.Buffer.Add('</ae><c>' + padSpace( 'desconto sobre item|' +
          FormatFloatBr(CFe.Det.Items[i].Prod.vDesc, '-,0.00'),
          FPosPrinter.ColunasFonteCondensada, '|'));
      end;

      // acrescimo
      if (CFe.Det.Items[i].Prod.vOutro > 0) then
      begin
        GapItem := True;
        FPosPrinter.Buffer.Add('</ae><c>' + padSpace( ACBrStr('acréscimo sobre item|') +
          FormatFloatBr(CFe.Det.Items[i].Prod.vOutro, '+,0.00'),
          FPosPrinter.ColunasFonteCondensada, '|'));
      end;

      // Rateio de Desconto
      if (CFe.Det.Items[i].Prod.vRatDesc > 0) then
      begin
        GapItem := True;
        FPosPrinter.Buffer.Add('</ae><c>' + padSpace( 'rateio de desconto sobre subtotal|' +
          FormatFloatBr(CFe.Det.Items[i].Prod.vRatDesc, '-,0.00'),
          FPosPrinter.ColunasFonteCondensada, '|'));
      end;

      // Rateio de Acréscimo
      if (CFe.Det.Items[i].Prod.vRatAcr > 0) then
      begin
        GapItem := True;
        FPosPrinter.Buffer.Add('</ae><c>' + padSpace( ACBrStr('rateio de acréscimo sobre subtotal|') +
          FormatFloatBr(CFe.Det.Items[i].Prod.vRatAcr, '+,0.00'),
          FPosPrinter.ColunasFonteCondensada, '|'));
      end;
    end;

    if (CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN > 0) then
    begin
      GapItem := True;
      FPosPrinter.Buffer.Add('</ae><c>' + PadSpace( ACBrStr('dedução para ISSQN|')+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN, '-,0.00'),
         FPosPrinter.ColunasFonteCondensada, '|'));
      FPosPrinter.Buffer.Add('</ae><c>' + PadSpace( ACBrStr('base de cálculo ISSQN|')+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vBC),
         FPosPrinter.ColunasFonteCondensada, '|'));
    end;

    if GapItem and (i < CFe.Det.Count-1) then
      FPosPrinter.Buffer.Add(' ');
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarTotais(Cancelamento: Boolean);
var
  TotalDescAcresItem: Double;
  Sinal: String;
begin
  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add('</ae></fn><n>TOTAL R$</n> '+ FormatFloatBr(CFe.Total.vCFe));
    Exit;
  end;

  TotalDescAcresItem := CFe.Total.ICMSTot.vOutro - CFe.Total.ICMSTot.vDesc;

  FPosPrinter.Buffer.Add('</ae><c> ');

  if (TotalDescAcresItem <> 0) or
     (CFe.Total.DescAcrEntr.vDescSubtot <> 0) or
     (CFe.Total.DescAcrEntr.vAcresSubtot <> 0) then
  begin
    FPosPrinter.Buffer.Add(PadSpace('Total Bruto de Itens|'+
       FormatFloatBr(CFe.Total.ICMSTot.vProd),
       FPosPrinter.ColunasFonteCondensada, '|'));
  end;

  if (TotalDescAcresItem <> 0) then
  begin
    Sinal := IfThen(TotalDescAcresItem < 0,'-','+');
    FPosPrinter.Buffer.Add(PadSpace(ACBrStr('Total de descontos/acréscimos sobre item|')+
       FormatFloatBr(Abs(TotalDescAcresItem), Sinal+',0.00'),
       FPosPrinter.ColunasFonteCondensada, '|'));
  end;

  if (CFe.Total.DescAcrEntr.vDescSubtot > 0) then
    FPosPrinter.Buffer.Add(PadSpace('Desconto sobre subtotal|'+
       FormatFloatBr(CFe.Total.DescAcrEntr.vDescSubtot, '-,0.00'),
       FPosPrinter.ColunasFonteCondensada, '|'));

  if (CFe.Total.DescAcrEntr.vAcresSubtot > 0) then
    FPosPrinter.Buffer.Add(PadSpace(ACBrStr('Acréscimo sobre subtotal|')+
       FormatFloatBr(CFe.Total.DescAcrEntr.vAcresSubtot, '+,0.00'),
       FPosPrinter.ColunasFonteCondensada, '|'));

  FPosPrinter.Buffer.Add('</ae></fn><e>'+PadSpace('TOTAL R$|'+
     FormatFloatBr(CFe.Total.vCFe),
     FPosPrinter.ColunasFonteExpandida, '|')+'</e>');
end;

procedure TACBrSATExtratoESCPOS.GerarPagamentos;
var
  i : integer;
begin
  FPosPrinter.Buffer.Add('</ae></fn> ');

  for i:=0 to CFe.Pagto.Count - 1 do
  begin
    FPosPrinter.Buffer.Add(ACBrStr(PadSpace(CodigoMPToDescricao(CFe.Pagto.Items[i].cMP)+'|'+
                FormatFloatBr(CFe.Pagto.Items[i].vMP),
                FPosPrinter.ColunasFonteNormal, '|')));
  end;

  if (CFe.Pagto.vTroco > 0) then
    FPosPrinter.Buffer.Add(PadSpace('Troco R$|'+
       FormatFloatBr(CFe.Pagto.vTroco),
       FPosPrinter.ColunasFonteNormal, '|'));
end;

procedure TACBrSATExtratoESCPOS.GerarObsFisco;
var
  i : integer;
begin
  if (CFe.InfAdic.obsFisco.Count > 0) or
     (CFe.Emit.cRegTrib = RTSimplesNacional) then
     FPosPrinter.Buffer.Add('</ae><c> ');

  if (CFe.Emit.cRegTrib = RTSimplesNacional) then
     FPosPrinter.Buffer.Add(Msg_ICMS_123_2006 );

  for i:=0 to CFe.obsFisco.Count - 1 do
     FPosPrinter.Buffer.Add(CFe.obsFisco[i].xCampo+'-'+
                            CFe.obsFisco[i].xTexto);
end;

procedure TACBrSATExtratoESCPOS.GerarDadosEntrega;
var
  TituloEntrega, LinhaEntrega: String;
begin
  if (Trim(CFe.Entrega.xLgr)+
      Trim(CFe.Entrega.nro)+
      Trim(CFe.Entrega.xCpl)+
      Trim(CFe.Entrega.xBairro)+
      Trim(CFe.Entrega.xMun) <> '') then
   begin
     TituloEntrega := ACBrStr('ENDEREÇO DE ENTREGA:');
     LinhaEntrega := QuebraLinhas( TituloEntrega+' '+Trim(CFe.Entrega.xLgr)+' '+
                     ifthen(Trim(CFe.Entrega.nro)<>'',', '+Trim(CFe.Entrega.nro),'') + ' ' +
                     ifthen(Trim(CFe.Entrega.xCpl)<>'',Trim(CFe.Entrega.xCpl) + ' ','') +
                     ifthen(Trim(CFe.Entrega.xBairro)<>'',Trim(CFe.Entrega.xBairro) + ' ','') +
                     Trim(CFe.Entrega.xMun)+'-'+CFe.Entrega.UF,
                     FPosPrinter.ColunasFonteCondensada);

     LinhaEntrega := AplicarAtributoTexto(LinhaEntrega, TituloEntrega, '<n>');

     FPosPrinter.Buffer.Add('</ae><c> ');
     FPosPrinter.Buffer.Add(LinhaEntrega);
   end;
end;

procedure TACBrSATExtratoESCPOS.GerarObsContribuinte(Resumido : Boolean = False );
var
  LininfCpl: String;
begin
  if (Trim(CFe.InfAdic.infCpl) <> '') or
     (ImprimeMsgOlhoNoImposto and (CFe.Total.vCFeLei12741 > 0)) then
  begin
    FPosPrinter.Buffer.Add('</ae></fn> ');
    FPosPrinter.Buffer.Add(ACBrStr('OBSERVAÇÕES DO CONTRIBUINTE'));

    if (Trim(CFe.InfAdic.infCpl) <> '') then
    begin
      LininfCpl := StringReplace(Trim(CFe.InfAdic.infCpl),';',sLineBreak,[rfReplaceAll]);
      LininfCpl := QuebraLinhas(LininfCpl, FPosPrinter.ColunasFonteCondensada);
      FPosPrinter.Buffer.Add('<c>'+LininfCpl);
    end;

    if not (SuportaQRCodeLateral or Resumido) then
      FPosPrinter.Buffer.Add('<c>*Valor aproximado dos tributos do item');

    if ImprimeMsgOlhoNoImposto and (CFe.Total.vCFeLei12741 > 0) then
    begin
      FPosPrinter.Buffer.Add('<c>'+PadSpace('Valor aproximado dos tributos deste cupom R$|<n>'+
                  FormatFloatBr(CFe.Total.vCFeLei12741)+'</n>',
                  FPosPrinter.ColunasFonteCondensada, '|'));
      FPosPrinter.Buffer.Add('(conforme Lei Fed. 12.741/2012)');
    end;
  end;
end;

function TACBrSATExtratoESCPOS.ComandosQRCode(const DadosQRCode: String): String;
begin
  Result := '<qrcode_tipo>2</qrcode_tipo>'+
            '<qrcode_error>0</qrcode_error>'+
            '<qrcode>'+DadosQRCode+'</qrcode>'
end;

procedure TACBrSATExtratoESCPOS.GerarRodape(Cancelamento: Boolean);
var
  DadosQRCode, Chave, TagCode128, ATexto, TituloConsumidor, TituloSAT,
    NomeConsumidor: String;
  ChaveEmUmaLinha, Suporta128c: Boolean;
  AlturaQRCode, AlturaMax, EsquerdaQRCode, ColunasDireira: Integer;
  SL: TStringList;
begin
  if not SuportaQRCodeLateral then
  begin
    if not Cancelamento then
      FPosPrinter.Buffer.Add('</fn></linha_simples>');

    FPosPrinter.Buffer.Add('</ce></fn>SAT No. <n>'+FormatFloatBr(CFe.ide.nserieSAT,'000,000,000')+'</n>');
    FPosPrinter.Buffer.Add(FormatDateTimeBr(CFe.ide.dEmi + CFe.ide.hEmi, 'DD/MM/YYYY - hh:nn:ss'));
  end
  else
     FPosPrinter.Buffer.Add(' ');

  // Imprimindo a Chave
  Chave := FormatarChaveAcesso(CFe.infCFe.ID);
  if Length(Chave) > FPosPrinter.ColunasFonteCondensada then
    Chave := OnlyNumber(Chave);

  if Length(Chave) > FPosPrinter.ColunasFonteCondensada then
  begin
    FPosPrinter.Buffer.Add(copy(CFe.infCFe.ID,1,22));
    FPosPrinter.Buffer.Add(copy(CFe.infCFe.ID,23,22));
  end
  else
    FPosPrinter.Buffer.Add('</ce><c>'+Chave);

  Suporta128c := (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) < 0);
  TagCode128 := IfThen(Suporta128c,'code128c', 'code128' );

  ChaveEmUmaLinha := (ImprimeChaveEmUmaLinha = rSim) or
                     ((ImprimeChaveEmUmaLinha = rAuto) and Suporta128c and
                      ((FPosPrinter.ColunasFonteNormal -2) >= Length(CFe.infCFe.ID)));

  if not ChaveEmUmaLinha then
  begin
    FPosPrinter.Buffer.Add('<'+TagCode128+'>'+copy(CFe.infCFe.ID,1,22)+'</'+TagCode128+'>');
    FPosPrinter.Buffer.Add('<'+TagCode128+'>'+copy(CFe.infCFe.ID,23,22)+'</'+TagCode128+'>');
  end
  else
    FPosPrinter.Buffer.Add('<'+TagCode128+'>'+CFe.infCFe.ID+'</'+TagCode128+'>');

  // Imprimindo o DadosQRCode
  if ImprimeQRCode then
  begin
    DadosQRCode := CalcularConteudoQRCode( CFe.infCFe.ID,
                                           CFe.ide.dEmi+CFe.ide.hEmi,
                                           CFe.Total.vCFe,
                                           Trim(CFe.Dest.CNPJCPF),
                                           CFe.ide.assinaturaQRCODE );

    if SuportaQRCodeLateral then
    begin
      ATexto := '';
      TituloConsumidor := 'Consumidor:';
      TituloSAT := ACBrStr('No.Série do SAT:');

      if (Trim(CFe.Dest.xNome) <> '') then
        NomeConsumidor := Trim(CFe.Dest.xNome)
      else if (ImprimeCPFNaoInformado and (CFe.Dest.CNPJCPF = '')) then
        NomeConsumidor := ACBrStr('NÃO IDENTIFICADO')
      else
        NomeConsumidor := '';

      if (Trim(Cfe.Dest.CNPJCPF) <> '') or (NomeConsumidor <> '') then
      begin
        ATexto := TituloConsumidor + ' '+
                  FormatarCNPJouCPF(CFe.Dest.CNPJCPF) +
                  IfThen((CFe.Dest.CNPJCPF <> '') and (NomeConsumidor <> ''), ' - ','') +
                  NomeConsumidor + sLineBreak;
      end;

      ATexto := ATexto + TituloSAT +
                FormatFloatBr(CFe.ide.nserieSAT,'000,000,000') + sLineBreak +
                FormatDateTimeBr(CFe.ide.dEmi + CFe.ide.hEmi, 'DD/MM/YYYY - hh:nn:ss') + sLineBreak;

      if (MsgAppQRCode <> '') then
        ATexto := ATexto + sLineBreak + MsgAppQRCode + sLineBreak;

      if (not Cancelamento) and ImprimeMsgOlhoNoImposto then
        ATexto := ATexto + sLineBreak + '*Valor aproximado dos tributos do item';

      ColunasDireira := trunc(FPosPrinter.ColunasFonteCondensada/2)-2;
      ATexto := QuebraLinhas( ATexto, ColunasDireira );
      ATexto := AplicarAtributoTexto( ATexto, TituloConsumidor, '<n>');
      ATexto := AplicarAtributoTexto( ATexto, TituloSAT, '<n>');

      SL := TStringList.Create;
      try
        SL.Text := ATexto;

        AlturaQRCode := FPosPrinter.CalcularAlturaQRCodeAlfaNumM(DadosQRCode);
        AlturaMax := max( FPosPrinter.CalcularAlturaTexto(SL.Count), AlturaQRCode );
        EsquerdaQRCode := Trunc(max(CLarguraRegiaoEsquerda - Trunc(AlturaQRCode/2),0) / 2);

        FPosPrinter.Buffer.Add( '<mp>' +
                                FPosPrinter.ConfigurarRegiaoModoPagina(
                                  EsquerdaQRCode, 0, AlturaMax,
                                  (CLarguraRegiaoEsquerda-EsquerdaQRCode) ) +
                                ComandosQRCode(DadosQRCode));
        FPosPrinter.Buffer.Add( FPosPrinter.ConfigurarRegiaoModoPagina(
                                  CLarguraRegiaoEsquerda, 0, AlturaMax, 325) +
                                '</ce><c>' + SL.Text + '</mp>');
      finally
        SL.Free;
      end;
    end
    else
    begin
      FPosPrinter.Buffer.Add(ComandosQRCode(DadosQRCode));

      if (not Cancelamento) and (MsgAppQRCode <> '') then
        FPosPrinter.Buffer.Add('</ce><c>' + QuebraLinhas(MsgAppQRCode, FPosPrinter.ColunasFonteCondensada ));
    end;
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarDadosCancelamento;
Var
  DadosQRCode, Chave, TagCode128 , TituloSAT, ATexto: String;
  ChaveEmUmaLinha, Suporta128c : Boolean;
  EsquerdaQRCode, AlturaQRCode, AlturaMax: Integer;
  SL : TStringList;
begin
  FPosPrinter.Buffer.Add('</fn></linha_simples>');
  FPosPrinter.Buffer.Add(ACBrStr('</ce><n>DADOS DO CUPOM FISCAL ELETRÔNICO DE CANCELAMENTO</n>'));

  if not SuportaQRCodeLateral then
  begin
    FPosPrinter.Buffer.Add('SAT No. <n>'+FormatFloatBr(CFeCanc.ide.nserieSAT,'000,000,000')+'</n>');
    FPosPrinter.Buffer.Add(FormatDateTimeBr(CFeCanc.ide.dEmi + CFeCanc.ide.hEmi, 'DD/MM/YYYY - hh:nn:ss'));
  end;

  Chave := FormatarChaveAcesso(CFeCanc.infCFe.ID);
  if Length(Chave) > FPosPrinter.ColunasFonteCondensada then
    Chave := OnlyNumber(Chave);

  FPosPrinter.Buffer.Add('<c>'+Chave);

  Suporta128c := (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) < 0);
  TagCode128 := IfThen(Suporta128c,'code128c', 'code128' );

  ChaveEmUmaLinha := (ImprimeChaveEmUmaLinha = rSim) or
                     ((ImprimeChaveEmUmaLinha = rAuto) and Suporta128c);

  if not ChaveEmUmaLinha then
  begin
    FPosPrinter.Buffer.Add('<' + TagCode128 + '>'+copy(CFeCanc.infCFe.ID,1,22)+'</' + TagCode128 + '>');
    FPosPrinter.Buffer.Add('<' + TagCode128 + '>'+copy(CFeCanc.infCFe.ID,23,22)+'</' + TagCode128 + '>');
  end
  else
    FPosPrinter.Buffer.Add('<' + TagCode128 + '>'+CFeCanc.infCFe.ID+'</' + TagCode128 + '>');


  if ImprimeQRCode then
  begin
    DadosQRCode := CalcularConteudoQRCode( CFeCanc.infCFe.ID,
                                      CFeCanc.ide.dEmi+CFeCanc.ide.hEmi,
                                      CFeCanc.Total.vCFe,
                                      Trim(CFeCanc.Dest.CNPJCPF),
                                      CFeCanc.ide.assinaturaQRCODE );

    if SuportaQRCodeLateral then
    begin
      TituloSAT := ACBrStr('No.Série do SAT:');
      ATexto := TituloSAT +
                FormatFloatBr(CFeCanc.ide.nserieSAT,'000,000,000') + sLineBreak +
                FormatDateTimeBr(CFeCanc.ide.dEmi + CFeCanc.ide.hEmi, 'DD/MM/YYYY - hh:nn:ss');

      ATexto := QuebraLinhas( ATexto, trunc(FPosPrinter.ColunasFonteCondensada/2) -2 );
      ATexto := AplicarAtributoTexto( ATexto, TituloSAT, '<n>');
      SL := TStringList.Create;
      try
        SL.Text := ATexto;
        AlturaQRCode   := FPosPrinter.CalcularAlturaQRCodeAlfaNumM(DadosQRCode);
        AlturaMax      := max( FPosPrinter.CalcularAlturaTexto(SL.Count), AlturaQRCode );
        EsquerdaQRCode := Trunc(max(CLarguraRegiaoEsquerda - Trunc(AlturaQRCode/2),0) / 2);

        FPosPrinter.Buffer.Add( '<mp>' +
                                FPosPrinter.ConfigurarRegiaoModoPagina(
                                  EsquerdaQRCode, 0, AlturaMax,
                                  (CLarguraRegiaoEsquerda-EsquerdaQRCode) ) +
                                ComandosQRCode(DadosQRCode));

        FPosPrinter.Buffer.Add( FPosPrinter.ConfigurarRegiaoModoPagina(
                                  CLarguraRegiaoEsquerda, 0, AlturaMax, 325) +
                                '</ce><c>' + SL.Text + '</mp>');
      finally
        SL.Free;
      end;
    end
    else
    begin
      FPosPrinter.Buffer.Add(ComandosQRCode(DadosQRCode));

      if (MsgAppQRCode <> '') then
        FPosPrinter.Buffer.Add('</ce><c>' + QuebraLinhas(MsgAppQRCode, FPosPrinter.ColunasFonteCondensada ));
    end;
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarFechamento;
var
  ATexto: String;
begin
  if (Sistema <> '') or (Site <> '') then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    ATexto := Trim(Sistema);

    if (Site <> '') then
    begin
      if (ATexto <> '') then
        ATexto := ATexto + ' - ';

      ATexto := ATexto + Site;
    end;

    ATexto := QuebraLinhas(ATexto, FPosPrinter.ColunasFonteCondensada);
    FPosPrinter.Buffer.Add('</ce><c>' + ATexto);
  end;

  if FPosPrinter.CortaPapel then
    FPosPrinter.Buffer.Add('</corte>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>');

  FPosPrinter.Buffer.Add('</reset>');
end;

function TACBrSATExtratoESCPOS.SuportaQRCodeLateral: Boolean;
begin
  Result := ImprimeQRCode and ImprimeQRCodeLateral and
            (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0);
end;

function TACBrSATExtratoESCPOS.SuportaLogoLateral: Boolean;
begin
  Result := (not FPosPrinter.ConfigLogo.IgnorarLogo) and ImprimeLogoLateral and
            (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0);
end;

procedure TACBrSATExtratoESCPOS.ImprimirCopias;
begin
  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrSATExtratoESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if (AValue <> FPosPrinter) then
  begin
    if Assigned(FPosPrinter) then
      FPosPrinter.RemoveFreeNotification(Self);

    FPosPrinter := AValue;

    if AValue <> nil then
      AValue.FreeNotification(self);
  end ;
end;

procedure TACBrSATExtratoESCPOS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
       FPosPrinter := nil ;
  end;
end;

procedure TACBrSATExtratoESCPOS.AtivarPosPrinter;
begin
  if not Assigned( FPosPrinter ) then
    raise Exception.Create('Componente PosPrinter não associado');

  FPosPrinter.Ativar;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;

  AtivarPosPrinter;

  GerarCabecalho;
  GerarItens;
  GerarTotais;
  GerarPagamentos;
  GerarObsFisco;
  GerarDadosEntrega;
  GerarObsContribuinte;
  GerarRodape;
  GerarFechamento;

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;

  AtivarPosPrinter;

  GerarCabecalho;
  GerarTotais;
  GerarPagamentos;
  GerarObsFisco;
  GerarDadosEntrega;
  GerarObsContribuinte(True);
  GerarRodape;
  GerarFechamento;

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;

  AtivarPosPrinter;

  GerarCabecalho(True);
  GerarTotais(True);
  GerarRodape(True);
  GerarDadosCancelamento;
  GerarFechamento;

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtrato(AStream: TStream; ACFe: TCFe);
begin
  raise EACBrSATErro.Create(cACBrSATStreamException);
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoCancelamento(AStream: TStream; ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  raise EACBrSATErro.Create(cACBrSATStreamException);
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoResumido(AStream: TStream; ACFe: TCFe);
begin
  raise EACBrSATErro.Create(cACBrSATStreamException);
end;

function TACBrSATExtratoESCPOS.GerarImpressaoFiscalMFe(ACFe: TCFe): String;
var
  OldImprimeQRCode : Boolean;
begin
  SetInternalCFe( ACFe );
  FLayOut := lCompleto;

  if (CFe = nil) or (CFe.infCFe.ID = '') then
    raise EACBrSATErro.Create( 'Nenhum CFe carregado na memória' ) ;

  OldImprimeQRCode := ImprimeQRCode;
  try
    ImprimeQRCode := False;

    GerarCabecalho;
    GerarItens;
    GerarTotais;
    GerarPagamentos;
    GerarObsFisco;
    GerarDadosEntrega;
    GerarObsContribuinte;
    GerarRodape;
    GerarFechamento;

    Result := StripHTML(FPosPrinter.Buffer.Text);
  finally
    FPosPrinter.Buffer.clear;
    ImprimeQRCode := OldImprimeQRCode;
  end;
end;

{$IFDEF FPC}
{$IFNDEF NOGUI}
initialization
   {$I ACBrSATExtratoESCPOS.lrs}
{$ENDIF}
{$ENDIF}

end.
