{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}
{$I ACBr.inc}

unit ACBrSATExtratoESCPOS;

interface

uses Classes, SysUtils,
     {$IFDEF FPC}
       LResources,
     {$ENDIF} 
     ACBrSATExtratoClass, ACBrPosPrinter,
     pcnCFe, pcnCFeCanc, pcnConversao;

type

  TAutoSimNao = (rAuto, rSim, rNao);

  { TACBrSATExtratoESCPOS }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrSATExtratoESCPOS = class( TACBrSATExtratoClass )
  private
    FBuffer:TStringList;
    FImprimeChaveEmUmaLinha: TAutoSimNao;
    FPosPrinter : TACBrPosPrinter ;

    procedure ImprimirCopias ;
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AtivarPosPrinter;

    procedure GerarCabecalho(Cancelamento: Boolean = False);
    procedure GerarItens;
    procedure GerarTotais(Resumido : Boolean = False);
    procedure GerarPagamentos(Resumido : Boolean = False );
    procedure GerarObsFisco;
    procedure GerarDadosEntrega;
    procedure GerarObsContribuinte(Resumido : Boolean = False );
    procedure GerarRodape(CortaPapel: Boolean = True; Cancelamento: Boolean = False);
    procedure GerarDadosCancelamento;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirExtrato(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
    function GerarImpressaoFiscalMFe(ACFe : TCFe = nil) : String;
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;

    property ImprimeChaveEmUmaLinha: TAutoSimNao read FImprimeChaveEmUmaLinha
      write FImprimeChaveEmUmaLinha default rAuto;

  end ;

procedure Register;

implementation

uses
  strutils, math,
  ACBrValidador, ACBrUtil, ACBrConsts, ACBrDFeUtil, ACBrSATClass;

{$IFNDEF FPC}
   {$R ACBrSATExtratoESCPOS.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrSAT',[TACBrSATExtratoESCPOS]);
end;

{ TACBrSATExtratoESCPOS }

constructor TACBrSATExtratoESCPOS.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FBuffer := TStringList.create;
  FPosPrinter := Nil;

  FImprimeChaveEmUmaLinha := rAuto;
end;

destructor TACBrSATExtratoESCPOS.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TACBrSATExtratoESCPOS.GerarCabecalho(Cancelamento: Boolean);
var
  nCFe, DocsEmit: String;
begin
  FBuffer.Clear;
  FBuffer.Add('</zera></ce></logo>');
  FBuffer.Add('<n>'+CFe.Emit.xFant+'</n>');

  FBuffer.Add('<c>'+CFe.Emit.xNome);
  FBuffer.Add(QuebraLinhas(Trim(CFe.Emit.EnderEmit.xLgr)+' '+
              Trim(CFe.Emit.EnderEmit.nro)+' '+
              Trim(CFe.Emit.EnderEmit.xCpl)+' '+
              Trim(CFe.Emit.EnderEmit.xBairro)+'-'+
              Trim(CFe.Emit.EnderEmit.xMun)+'-'+
              FormatarCEP(CFe.Emit.EnderEmit.CEP),FPosPrinter.ColunasFonteCondensada));

  DocsEmit := 'CNPJ:'+FormatarCNPJ(CFe.Emit.CNPJ)+
                  ' IE:'+Trim(CFe.Emit.IE);

  { Verifica se existe valor no campo IM }
  if (CFe.Emit.IM <> '') and (CFe.Emit.IM <> '0') then
    DocsEmit := DocsEmit + ' IM:'+Trim(CFe.Emit.IM);

  FBuffer.Add( '</ce><c>'+ DocsEmit);
  FBuffer.Add('</linha_simples>');


  if CFe.ide.tpAmb = taHomologacao then
  begin
    FBuffer.Add('</fn></ce><n>Extrato No. 000000');
    FBuffer.Add(ACBrStr('CUPOM FISCAL ELETRÔNICO - SAT</n>'));
    FBuffer.Add(' ');
    FBuffer.Add(' = T E S T E =');
    FBuffer.Add(' ');
    FBuffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FBuffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
    FBuffer.Add(StringOfChar('>',FPosPrinter.ColunasFonteNormal));
  end
  else
  begin
    if (Cancelamento) then
      nCFe := IntToStrZero( CFeCanc.ide.nCFe, 6)
    else
      nCFe := IntToStrZero( CFE.ide.nCFe, 6);

    FBuffer.Add('</fn></ce><n>Extrato No. '+ nCFe );
    FBuffer.Add( ACBrStr('CUPOM FISCAL ELETRÔNICO - SAT</n>'));
  end;

  if (Trim(Cfe.Dest.CNPJCPF) <> '') or ImprimeCPFNaoInformado then
  begin
    FBuffer.Add('</linha_simples>');
    FBuffer.Add('</ae><c>CPF/CNPJ do Consumidor: '+
                IfThen( Trim(CFe.Dest.CNPJCPF)<>'',
                        FormatarCNPJouCPF(CFe.Dest.CNPJCPF),
                        ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')));
  end;

  if Trim(CFe.Dest.xNome) <> '' then
    FBuffer.Add( ACBrStr('Razão Social/Nome: ')+CFe.Dest.xNome );
end;

procedure TACBrSATExtratoESCPOS.GerarItens;
var
  i: Integer;
  nTamDescricao: Integer;
  fQuant, VlrLiquido: Double;
  sItem, sCodigo, sDescricao, sQuantidade, sUnidade, sVlrUnitario, sVlrProduto,
    sVlrImpostos, LinhaCmd: String;
begin
  FBuffer.Add('</ae><c></linha_simples>');
  FBuffer.Add(PadSpace('#|COD|DESC|QTD|UN|VL UN R$|(VLTR R$)*|VL ITEM R$',
                                  FPosPrinter.ColunasFonteCondensada, '|'));
  FBuffer.Add('</linha_simples>');

  for i := 0 to CFe.Det.Count - 1 do
  begin
    sItem        := IntToStrZero(CFe.Det.Items[i].nItem, 3);
    sDescricao   := Trim(CFe.Det.Items[i].Prod.xProd);
    sUnidade     := Trim(CFe.Det.Items[i].Prod.uCom);
    sVlrProduto  := FormatFloatBr(CFe.Det.Items[i].Prod.vProd);

    if (Length( Trim( CFe.Det.Items[i].Prod.cEAN ) ) > 0) and (UsaCodigoEanImpressao) then
      sCodigo := Trim(CFe.Det.Items[i].Prod.cEAN)
    else
      sCodigo := Trim(CFe.Det.Items[i].Prod.cProd);

    // formatar conforme configurado
    sVlrUnitario := FormatFloatBr(CFe.Det.Items[i].Prod.vUnCom,
      IfThen(CFe.Det.Items[i].Prod.EhCombustivel, ',0.000', Mask_vUnCom));
    if CFe.Det.Items[i].Imposto.vItem12741 > 0 then
      sVlrImpostos := ' ('+FormatFloatBr(CFe.Det.Items[i].Imposto.vItem12741)+') '
    else
      sVlrImpostos := ' ';

    // formatar conforme configurado somente quando houver decimais
    // caso contrário mostrar somente o número inteiro
    fQuant := CFe.Det.Items[i].Prod.QCom;
    if Frac(fQuant) > 0 then
      sQuantidade := FormatFloatBr(fQuant, Mask_qCom )
    else
      sQuantidade := FloatToStr(fQuant);

    if ImprimeEmUmaLinha then
    begin
      LinhaCmd := sItem + ' ' + sCodigo + ' ' + '[DesProd] ' + sQuantidade + ' ' +
        sUnidade + ' X ' + sVlrUnitario + sVlrImpostos + sVlrProduto;

      // acerta tamanho da descrição
      nTamDescricao := FPosPrinter.ColunasFonteCondensada - Length(LinhaCmd) + 9;
      sDescricao := PadRight(Copy(sDescricao, 1, nTamDescricao), nTamDescricao);

      LinhaCmd := StringReplace(LinhaCmd, '[DesProd]', sDescricao, [rfReplaceAll]);
      FBuffer.Add('</ae><c>' + LinhaCmd);
    end
    else
    begin
      LinhaCmd := sItem + ' ' + sCodigo + ' ' + sDescricao;
      FBuffer.Add('</ae><c>' + LinhaCmd);

      LinhaCmd :=
        PadRight(sQuantidade, 15) + ' ' + PadRight(sUnidade, 6) + ' X ' +
        PadRight(sVlrUnitario, 13) + '|' + sVlrImpostos + sVlrProduto;

      LinhaCmd := padSpace(LinhaCmd, FPosPrinter.ColunasFonteCondensada, '|');
      FBuffer.Add('</ae><c>' + LinhaCmd);
    end;

    if ImprimeDescAcrescItem then
    begin
      VlrLiquido := CFe.Det.Items[i].Prod.vProd;

      // desconto
      if CFe.Det.Items[i].Prod.vDesc > 0 then
      begin
        VlrLiquido := VlrLiquido - CFe.Det.Items[i].Prod.vDesc;

        LinhaCmd := '</ae><c>' + padSpace(
            'desconto ' + padLeft(FormatFloatBr(CFe.Det.Items[i].Prod.vDesc, '-,0.00'), 15, ' ')
            + '|' + FormatFloatBr(VlrLiquido),
            FPosPrinter.ColunasFonteCondensada, '|');
        FBuffer.Add('</ae><c>' + LinhaCmd);
      end;

      // ascrescimo
      if CFe.Det.Items[i].Prod.vOutro > 0 then
      begin
        VlrLiquido := VlrLiquido + CFe.Det.Items[i].Prod.vOutro;

        LinhaCmd := '</ae><c>' + ACBrStr(padSpace(
            'acréscimo ' + padLeft(FormatFloatBr(CFe.Det.Items[i].Prod.vOutro, '+,0.00'), 15, ' ')
            + '|' + FormatFloatBr(VlrLiquido),
            FPosPrinter.ColunasFonteCondensada, '|'));
        FBuffer.Add('</ae><c>' + LinhaCmd);
      end;
    end;

    if CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN > 0 then
    begin
      FBuffer.Add(ACBrStr(PadSpace('Dedução para ISSQN|'+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vDeducISSQN, '-,0.00'),
         FPosPrinter.ColunasFonteCondensada, '|')));
      FBuffer.Add(ACBrStr(PadSpace('Base de cálculo ISSQN|'+
         FormatFloatBr(CFe.Det.Items[i].Imposto.ISSQN.vBC),
         FPosPrinter.ColunasFonteCondensada, '|')));
    end;

  end;
end;

procedure TACBrSATExtratoESCPOS.GerarTotais(Resumido: Boolean);
var
  Descontos, Acrescimos: Double;
begin
  if not Resumido then
   begin
     Descontos  := (CFe.Total.ICMSTot.vDesc  + CFe.Total.DescAcrEntr.vDescSubtot);
     Acrescimos := (CFe.Total.ICMSTot.vOutro + CFe.Total.DescAcrEntr.vAcresSubtot);

     if (Descontos > 0) or (Acrescimos > 0) then
        FBuffer.Add('<c>'+PadSpace('Subtotal|'+
           FormatFloatBr(CFe.Total.ICMSTot.vProd),
           FPosPrinter.ColunasFonteCondensada, '|'));

     if Descontos > 0 then
        FBuffer.Add('<c>'+PadSpace('Descontos|'+
           FormatFloatBr(Descontos, '-,0.00'),
           FPosPrinter.ColunasFonteCondensada, '|'));

     if Acrescimos > 0 then
        FBuffer.Add('<c>'+ACBrStr(PadSpace('Acréscimos|'+
           FormatFloatBr(Acrescimos, '+,0.00'),
           FPosPrinter.ColunasFonteCondensada, '|')));
   end;

  FBuffer.Add('</ae></fn><e>'+PadSpace('TOTAL R$|'+
     FormatFloatBr(CFe.Total.vCFe),
     trunc(FPosPrinter.ColunasFonteExpandida), '|')+'</e>');
end;

procedure TACBrSATExtratoESCPOS.GerarPagamentos(Resumido : Boolean = False );
var
  i : integer;
begin
  {if not Resumido then
    FBuffer.Add('');  }

  for i:=0 to CFe.Pagto.Count - 1 do
  begin
    FBuffer.Add('<c>'+ACBrStr(PadSpace(CodigoMPToDescricao(CFe.Pagto.Items[i].cMP)+'|'+
                FormatFloatBr(CFe.Pagto.Items[i].vMP),
                FPosPrinter.ColunasFonteCondensada, '|')));
  end;

  if CFe.Pagto.vTroco > 0 then
    FBuffer.Add('<c>'+PadSpace('Troco R$|'+
       FormatFloatBr(CFe.Pagto.vTroco),
       FPosPrinter.ColunasFonteCondensada, '|'));
end;

procedure TACBrSATExtratoESCPOS.GerarObsFisco;
var
  i : integer;
begin
  if (CFe.InfAdic.obsFisco.Count > 0) or
     (CFe.Emit.cRegTrib = RTSimplesNacional) then
     FBuffer.Add('<c> ');

  if CFe.Emit.cRegTrib = RTSimplesNacional then
     FBuffer.Add('<c>' + Msg_ICMS_123_2006 );

  for i:=0 to CFe.InfAdic.obsFisco.Count - 1 do
     FBuffer.Add('<c>'+CFe.InfAdic.obsFisco.Items[i].xCampo+'-'+
                                  CFe.InfAdic.obsFisco.Items[i].xTexto);
end;

procedure TACBrSATExtratoESCPOS.GerarDadosEntrega;
begin
  if Trim(CFe.Entrega.xLgr)+
     Trim(CFe.Entrega.nro)+
     Trim(CFe.Entrega.xCpl)+
     Trim(CFe.Entrega.xBairro)+
     Trim(CFe.Entrega.xMun) <> '' then
   begin
     FBuffer.Add('</fn></linha_simples>');
     FBuffer.Add('DADOS PARA ENTREGA');

     if Trim(CFe.Entrega.xLgr)+
        Trim(CFe.Entrega.nro)+
        Trim(CFe.Entrega.xCpl)+
        Trim(CFe.Entrega.xBairro)+
        Trim(CFe.Entrega.xMun) <> '' then
     begin
        FBuffer.Add('<c>'+Trim(CFe.Entrega.xLgr)+' '+
                    Trim(CFe.Entrega.nro)+' '+
                    Trim(CFe.Entrega.xCpl)+' '+
                    Trim(CFe.Entrega.xBairro)+' '+
                    Trim(CFe.Entrega.xMun));
     end;
     FBuffer.Add(CFe.Dest.xNome);
   end;
end;

procedure TACBrSATExtratoESCPOS.GerarObsContribuinte(Resumido : Boolean = False );
var
  CabecalhoGerado: Boolean;

  procedure GerarCabecalhoObsContribuinte;
  begin
    FBuffer.Add('</fn></linha_simples>');
    FBuffer.Add(ACBrStr('OBSERVAÇÕES DO CONTRIBUINTE'));
    CabecalhoGerado := True;
  end;

begin
  CabecalhoGerado := False;

  if Trim(CFe.InfAdic.infCpl) <> '' then
  begin
    GerarCabecalhoObsContribuinte;
    FBuffer.Add('<c>'+StringReplace(Trim(CFe.InfAdic.infCpl),';',sLineBreak,[rfReplaceAll]));
  end;

  if ImprimeMsgOlhoNoImposto and (CFe.Total.vCFeLei12741 > 0) then
  begin
    if not CabecalhoGerado then
      GerarCabecalhoObsContribuinte;
//    else
//      FBuffer.Add(' ');

    if not Resumido then
      FBuffer.Add('<c>*Valor aproximado dos tributos do item');

    FBuffer.Add('<c>'+PadSpace('Valor aproximado dos tributos deste cupom R$ |<n>'+
                FormatFloatBr(CFe.Total.vCFeLei12741),
                FPosPrinter.ColunasFonteCondensada, '|'));
    FBuffer.Add('</n>(conforme Lei Fed. 12.741/2012)');
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarRodape(CortaPapel: Boolean = True; Cancelamento: Boolean = False);
var
  QRCode, Chave, TagCode128: String;
  ChaveEmUmaLinha, Suporta128c: Boolean;
begin
  FBuffer.Add('</fn></linha_simples>');
  if Cancelamento then
     FBuffer.Add(ACBrStr('</ce><n>DADOS DO CUPOM FISCAL ELETRÔNICO CANCELADO</n>'));

  Chave := FormatarChaveAcesso(CFe.infCFe.ID);
  if Length(Chave) > FPosPrinter.ColunasFonteCondensada then
    Chave := OnlyNumber(Chave);

  FBuffer.Add('</ce>SAT No. <n>'+IntToStr(CFe.ide.nserieSAT)+'</n>');
  FBuffer.Add(FormatDateTimeBr(CFe.ide.dEmi + CFe.ide.hEmi));

  if not FPosPrinter.ConfigBarras.MostrarCodigo then
    FBuffer.Add('<c>'+Chave+'</fn>');

  Suporta128c := (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) < 0);
  TagCode128 := IfThen(Suporta128c,'code128c', 'code128' );

  ChaveEmUmaLinha := (ImprimeChaveEmUmaLinha = rSim) or
                     ((ImprimeChaveEmUmaLinha = rAuto) and Suporta128c and
                      ((FPosPrinter.ColunasFonteNormal -2) >= Length(CFe.infCFe.ID)));

  if not ChaveEmUmaLinha then
  begin
    FBuffer.Add('<'+TagCode128+'>'+copy(CFe.infCFe.ID,1,22)+'</'+TagCode128+'>');
    FBuffer.Add('<'+TagCode128+'>'+copy(CFe.infCFe.ID,23,22)+'</'+TagCode128+'>');
  end
  else
    FBuffer.Add('<'+TagCode128+'>'+CFe.infCFe.ID+'</'+TagCode128+'>');

  if ImprimeQRCode then
  begin
    QRCode := CalcularConteudoQRCode( CFe.infCFe.ID,
                                      CFe.ide.dEmi+CFe.ide.hEmi,
                                      CFe.Total.vCFe,
                                      Trim(CFe.Dest.CNPJCPF),
                                      CFe.ide.assinaturaQRCODE );

    FBuffer.Add('<qrcode_tipo>2</qrcode_tipo>'+
                '<qrcode_error>0</qrcode_error>'+
                '<qrcode>'+QRCode+'</qrcode>');
  end;

  if not Cancelamento then
  begin
    if MsgAppQRCode <> '' then
      FBuffer.Add('</ce><c>' + QuebraLinhas(MsgAppQRCode, FPosPrinter.ColunasFonteCondensada ));

    if (SoftwareHouse <> '') or (Site <> '') then
      FBuffer.Add('</linha_simples>');

    // SoftwareHouse
    if SoftwareHouse <> '' then
      FBuffer.Add('</ce><c>' + SoftwareHouse);

    if Site <> '' then
      FBuffer.Add('</ce><c>' + Site);
  end;

  FBuffer.Add('</zera>');
  
  if CortaPapel then
  begin
    if FPosPrinter.CortaPapel then
      FBuffer.Add('</corte_total>')
    else
      FBuffer.Add('</pular_linhas>');
  end;
end;

procedure TACBrSATExtratoESCPOS.GerarDadosCancelamento;
Var
  ChaveEmUmaLinha, Suporta128c : Boolean;
  Chave, TagCode128 : String;
  QRCode: AnsiString;
begin
  FBuffer.Add('</fn></linha_simples>');
  FBuffer.Add(ACBrStr('</ce><n>DADOS DO CUPOM FISCAL ELETRÔNICO DE CANCELAMENTO</n>'));
  FBuffer.Add('</ce>SAT No. <n>'+IntToStr(CFe.ide.nserieSAT)+'</n>');
  FBuffer.Add(FormatDateTimeBr(CFeCanc.ide.dEmi + CFeCanc.ide.hEmi));

  Chave := FormatarChaveAcesso(CFeCanc.infCFe.ID);
  if Length(Chave) > FPosPrinter.ColunasFonteCondensada then
    Chave := OnlyNumber(Chave);

  if not FPosPrinter.ConfigBarras.MostrarCodigo then
    FBuffer.Add('<c>'+Chave+'</fn>');

  Suporta128c := (FPosPrinter.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) < 0);
  TagCode128 := IfThen(Suporta128c,'code128c', 'code128' );

  ChaveEmUmaLinha := (ImprimeChaveEmUmaLinha = rSim) or
                     ((ImprimeChaveEmUmaLinha = rAuto) and Suporta128c);

  if not ChaveEmUmaLinha then
  begin
    FBuffer.Add('<' + TagCode128 + '>'+copy(CFeCanc.infCFe.ID,1,22)+'</' + TagCode128 + '>');
    FBuffer.Add('<' + TagCode128 + '>'+copy(CFeCanc.infCFe.ID,23,22)+'</' + TagCode128 + '>');
  end
  else
    FBuffer.Add('<' + TagCode128 + '>'+CFeCanc.infCFe.ID+'</' + TagCode128 + '>');


  if ImprimeQRCode then
  begin
    QRCode := CalcularConteudoQRCode( CFeCanc.infCFe.ID,
                                      CFeCanc.ide.dEmi+CFeCanc.ide.hEmi,
                                      CFeCanc.Total.vCFe,
                                      Trim(CFeCanc.Dest.CNPJCPF),
                                      CFeCanc.ide.assinaturaQRCODE );

    FBuffer.Add('<qrcode_tipo>2</qrcode_tipo>'+
                '<qrcode_error>0</qrcode_error>'+
                '<qrcode>'+QRCode+'</qrcode>');
  end;

  if MsgAppQRCode <> '' then
    FBuffer.Add('</ce><c>' + QuebraLinhas(MsgAppQRCode, FPosPrinter.ColunasFonteCondensada ));

  if (SoftwareHouse <> '') or (Site <> '') then
    FBuffer.Add('</linha_simples>');

  // SoftwareHouse
  if SoftwareHouse <> '' then
    FBuffer.Add('</ce><c>' + SoftwareHouse);

  if Site <> '' then
    FBuffer.Add('</ce><c>' + Site);

  FBuffer.Add('</zera>');	

  if FPosPrinter.CortaPapel then
    FBuffer.Add('</corte_total>')
  else
    FBuffer.Add('</pular_linhas>');
end;

procedure TACBrSATExtratoESCPOS.ImprimirCopias;
var
  I:integer;
  N:Integer;
begin
  for N := 1 to NumCopias do
  begin
    for I := 0 to FBuffer.Count-1 do
      FPosPrinter.Buffer.Add(FBuffer[I]);

    FPosPrinter.Imprimir();   // Imprime o Buffer
  end;
  FBuffer.clear;
end;

procedure TACBrSATExtratoESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
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

  ImprimirCopias;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;

  AtivarPosPrinter;

  GerarCabecalho(True);
  GerarTotais(True);
  GerarRodape(False, True);
  GerarDadosCancelamento;

  ImprimirCopias;
end;

function TACBrSATExtratoESCPOS.GerarImpressaoFiscalMFe(ACFe: TCFe): String;
var
  OldImprimeQRCode : Boolean;
begin
  SetInternalCFe( ACFe );
  fpLayOut := lCompleto;

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

    Result := StripHTML(FBuffer.Text);
  finally
    FBuffer.clear;
    ImprimeQRCode := OldImprimeQRCode;
  end;
end;

procedure TACBrSATExtratoESCPOS.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;

  AtivarPosPrinter;

  GerarCabecalho;
  GerarTotais(True);
  GerarPagamentos(True);
  GerarObsFisco;
  GerarDadosEntrega;
  GerarObsContribuinte(True);
  GerarRodape;

  ImprimirCopias;
end;

{$IFDEF FPC}
{$IFNDEF NOGUI}
initialization
   {$I ACBrSATExtratoESCPOS.lrs}
{$ENDIF}
{$ENDIF}

end.
