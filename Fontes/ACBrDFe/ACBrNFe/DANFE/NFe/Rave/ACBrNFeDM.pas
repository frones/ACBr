{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 09/03/2009: Dulcemar P.Zilli
|*  - Correcao impressão informações IPI
|* 11/03/2008: Dulcemar P.Zilli
|*  - Inclusão Observações Fisco
|* 11/03/2008: Dulcemar P.Zilli
|*  - Inclusão totais ISSQN
|* 23/06/2009: João H. Souza
|*  - Alterações diversas
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDM;

interface

uses
  Forms, SysUtils, Classes,
  RpRave, RpBase, RpSystem, RpDefine, RpCon, RpRender, RpRenderPDF,
  pcnNFe, pcnConversao;

type
  TdmACBrNFe = class( TDataModule )
   private
    FNFe : TNFe; 
    FLogo: String;
    FSistema:String;
    FUsuario:String;
    FPathArquivos : String;
    FImpressora : String;
    FImprimirHoraSaida : Boolean;
    FImprimirDataEmissao : Boolean;
    FMostrarPreview : Boolean;
    RvSystem1: TRvSystem;
    RvProject: TRvProject;
    RvRenderPDF1: TRvRenderPDF;
    CustomDestinatarioCXN: TRvCustomConnection;
    CustomEmitenteCXN: TRvCustomConnection;
    CustomDadosProdutosCXN: TRvCustomConnection;
    CustomTransportadorCXN: TRvCustomConnection;
    CustomCalculoImpostoCXN: TRvCustomConnection;
    CustomParametrosCXN: TRvCustomConnection;
    CustomDuplicatasCXN: TRvCustomConnection;
    CustomIdentificacaoCXN: TRvCustomConnection;
    CustomVeiculoCXN: TRvCustomConnection;
    CustomVolumesCXN: TRvCustomConnection;
    CustomInformacoesAdicionaisCXN: TRvCustomConnection;
    procedure SetNFE(const Value: TComponent);
    procedure RvSystem1BeforePrint(Sender: TObject);
    procedure CustomDestinatarioCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomDestinatarioCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomDestinatarioCXNOpen(Connection: TRvCustomConnection);
    procedure CustomEmitenteCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomEmitenteCXNOpen(Connection: TRvCustomConnection);
    procedure CustomEmitenteCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomCalculoImpostoCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomCalculoImpostoCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomCalculoImpostoCXNOpen(Connection: TRvCustomConnection);
    procedure CustomDadosProdutosCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomDadosProdutosCXNOpen(Connection: TRvCustomConnection);
    procedure CustomDadosProdutosCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomTransportadorCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomTransportadorCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomTransportadorCXNOpen(Connection: TRvCustomConnection);
    procedure CustomParametrosCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomParametrosCXNOpen(Connection: TRvCustomConnection);
    procedure CustomParametrosCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomIdentificacaoCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomIdentificacaoCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomIdentificacaoCXNOpen(Connection: TRvCustomConnection);
    procedure CustomDuplicatasCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomDuplicatasCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomDuplicatasCXNOpen(Connection: TRvCustomConnection);
    procedure CustomVeiculoCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomVeiculoCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomVeiculoCXNOpen(Connection: TRvCustomConnection);
    procedure CustomVolumesCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomVolumesCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomVolumesCXNOpen(Connection: TRvCustomConnection);
    procedure CustomInformacoesAdicionaisCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomInformacoesAdicionaisCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomInformacoesAdicionaisCXNOpen(Connection: TRvCustomConnection);
    procedure CustomObservacaoFiscoCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomObservacaoFiscoCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomObservacaoFiscoCXNOpen(Connection: TRvCustomConnection);
    procedure CustomISSQNCXNGetCols(Connection: TRvCustomConnection);
    procedure CustomISSQNCXNGetRow(Connection: TRvCustomConnection);
    procedure CustomISSQNCXNOpen(Connection: TRvCustomConnection);
    procedure CustomEmitenteCXNEOF(Connection: TRvCustomConnection;
      var Eof: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Imprimir(ANFe: TNFe; ALogo: String = '');
    procedure ImprimirPDF(ANFe: TNFe; ALogo: String = '');

  published
    property ACBrNFe : TComponent  read FNFe write SetNFE ;
    property Logo: String read FLogo write FLogo ;
    property Sistema: String read FSistema write FSistema ;
    property Usuario: String read FUsuario write FUsuario ;
    property PathArquivos: String read FPathArquivos write FPathArquivos ;
    property Impressora: String read FImpressora write FImpressora ;
    property ImprimirHoraSaida: Boolean read FImprimirHoraSaida write FImprimirHoraSaida ;
    property ImprimirDataEmissao: Boolean read FImprimirDataEmissao write FImprimirDataEmissao ;
    property MostrarPreview: Boolean read FMostrarPreview write FMostrarPreview ;
  end;

implementation

uses ACBrNFe, ACBrDFeUtil, StrUtils ;

{$R *.dfm}

function explode(sPart, sInput: string): array of string;
begin
  while Pos(sPart, sInput) <> 0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Copy(sInput, 0, Pos(sPart, sInput) - 1);
      Delete(sInput, 1, Pos(sPart, sInput));
    end;

  SetLength(Result, Length(Result) + 1);
  Result[Length(Result) - 1] := sInput;
end;

function implode(sPart:string; tokens:array of string):string;
var i:integer;
begin
   result := '';
   for i := 0 to Length(tokens) - 1 do
     result:=result+(tokens[i])+sPart;
end;

constructor TdmACBrNFe.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FNFe          := nil ;
  FLogo         := '' ;
  FSistema      := '' ;
  FUsuario      := '' ;
  FPathArquivos := '' ;
  FImpressora   := '' ;
  FImprimirHoraSaida    := False;
  FImprimirDataEmissao  := False;
  FMostrarPreview       := True;
end;

destructor TdmACBrNFe.Destroy;
begin

  inherited Destroy ;
end;

procedure TdmACBrNFe.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FNFe <> nil) and (AComponent is TACBrNFe) then
     FNFe := nil ;
end;

procedure TdmACBrNFe.SetNFE(const Value: TComponent);
  Var OldValue : TACBrNFe ;
begin
  if Value <> FNFe then
  begin
     if Value <> nil then
        if not (Value is TACBrNFe) then
           raise EACBrNFeException.Create('ACBrDANFERave.NFE deve ser do tipo TACBrNFe') ;

     if Assigned(FNFe) then
        FNFe.RemoveFreeNotification(Self);

     OldValue := TACBrNFe(FNFe) ;   // Usa outra variavel para evitar Loop Infinito
     FNFe := Value;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.NFE) then
           OldValue.DANFE := nil ;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrNFe(Value).DANFE := self ;
     end ;
  end ;
end;

procedure TdmACBrNFe.Imprimir(ANFe: TNFe; ALogo: String);
begin
  FNFe  := ANFe;
  FLogo := ALogo;
  RvProject.ProjectFile := ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.rav';
  RvProject.Execute;
end;

procedure TdmACBrNFe.ImprimirPDF(ANFe: TNFe; ALogo: String);
begin
  FNFe  := ANFe;
  FLogo := ALogo;

  RvSystem1.DefaultDest := rdFile;
  RvSystem1.DoNativeOutput:=false;
  RvSystem1.RenderObject:= RvRenderPDF1;
  RvSystem1.OutputFileName:=  ExtractFileDir(application.ExeName)+'\'+FNFe.InfNFe.Id+'.pdf';
  RvSystem1.SystemSetups:=RvSystem1.SystemSetups - [ssAllowSetup];
  RvProject.Engine := RvSystem1;

  RvProject.ProjectFile := ExtractFileDir(application.ExeName)+'\Report\NotaFiscalEletronica.rav';
  RvProject.Execute;

  RvSystem1.DoNativeOutput := True;
  RvSystem1.DefaultDest    := rdPrinter;
  RvSystem1.RenderObject   := nil;
  RvSystem1.OutputFileName := '';
  RvSystem1.SystemSetups:=RvSystem1.SystemSetups + [ssAllowSetup];
end;

procedure TdmACBrNFe.RvSystem1BeforePrint(Sender: TObject);
begin
   if (FNFe.Ide.tpEmis=teNormal) then
      RvProject.SetParam('Contigencia','')
   else
      RvProject.SetParam('Contigencia','DANFE em contigência, impresso em decorrência de problemas técnicos');
end;

procedure TdmACBrNFe.CustomDestinatarioCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('CNPJCPF',dtString,18,'','');
  Connection.WriteField('XNome',dtString,60,'','');
  Connection.WriteField('XLgr',dtString,60,'','');
  Connection.WriteField('Nro',dtString,60,'','');
  Connection.WriteField('XCpl',dtString,60,'','');
  Connection.WriteField('XBairro',dtString,60,'','');
  Connection.WriteField('CMun',dtString,7,'','');
  Connection.WriteField('XMun',dtString,60,'','');
  Connection.WriteField('UF',dtString,2,'','');
  Connection.WriteField('CEP',dtString,9,'','');
  Connection.WriteField('CPais',dtString,4,'','');
  Connection.WriteField('XPais',dtString,60,'','');
  Connection.WriteField('Fone',dtString,15,'','');
  Connection.WriteField('IE',dtString,14,'','');
end;

procedure TdmACBrNFe.CustomDestinatarioCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Dest do
  begin
    if NaoEstaVazio(CNPJCPF) then
      Connection.WriteStrData('', FormatarCNPJ(CNPJCPF))
    else
      Connection.WriteStrData('', FormatarCPF(CNPJCPF));

    Connection.WriteStrData('', XNome);
    with EnderDest do
    begin
      Connection.WriteStrData('', XLgr);
      Connection.WriteStrData('', Nro);
      Connection.WriteStrData('', XCpl);
      Connection.WriteStrData('', XBairro);
      Connection.WriteStrData('', inttostr(CMun));
      Connection.WriteStrData('', ParseText(XMun,true));
      Connection.WriteStrData('', UF);
      Connection.WriteStrData('', FormatarCEP(inttostr(CEP)));
      Connection.WriteStrData('', inttostr(CPais));
      Connection.WriteStrData('', XPais);
      Connection.WriteStrData('', FormatarFone(Fone));
    end;
    Connection.WriteStrData('', IE);
  end;

end;

procedure TdmACBrNFe.CustomDestinatarioCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomEmitenteCXNGetCols(
  Connection: TRvCustomConnection);
begin
  //emit
  Connection.WriteField('CNPJ', dtString, 18, '', '');
  Connection.WriteField('XNome', dtString, 60, '', '');
  Connection.WriteField('XFant', dtString, 60, '', '');
  Connection.WriteField('XLgr', dtString, 60, '', '');
  Connection.WriteField('Nro', dtString, 60, '', '');
  Connection.WriteField('XCpl', dtString, 60, '', '');
  Connection.WriteField('XBairro', dtString, 60, '', '');
  Connection.WriteField('CMun', dtString, 7, '', '');
  Connection.WriteField('XMun', dtString, 60, '', '');
  Connection.WriteField('UF', dtString, 2, '', '');
  Connection.WriteField('CEP', dtString, 9, '', '');
  Connection.WriteField('CPais', dtString, 4, '', '');
  Connection.WriteField('XPais', dtString, 60, '', '');
  Connection.WriteField('Fone', dtString, 15, '', '');
  Connection.WriteField('IE', dtString, 14, '', '');

  Connection.WriteField('IM', dtString, 15, '', '');
  Connection.WriteField('IEST', dtString, 15, '', '');

end;

procedure TdmACBrNFe.CustomEmitenteCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomEmitenteCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Emit do
  begin
    Connection.WriteStrData('', FormatarCNPJ(CNPJCPF));
    Connection.WriteStrData('', XNome);
    Connection.WriteStrData('', XFant);
    with EnderEmit do
    begin
      Connection.WriteStrData('', XLgr);
      Connection.WriteStrData('', Nro);
      Connection.WriteStrData('', XCpl);
      Connection.WriteStrData('', XBairro);
      Connection.WriteStrData('', inttostr(CMun));
      Connection.WriteStrData('', ParseText(XMun,true));
      Connection.WriteStrData('', UF);
      Connection.WriteStrData('', FormatarCEP(inttostr(CEP)));
      Connection.WriteStrData('', inttostr(CPais));
      Connection.WriteStrData('', XPais);
      Connection.WriteStrData('', NotaUtil.FormatarFone(Fone));
    end;
    Connection.WriteStrData('', IE);
    Connection.WriteStrData('', IM);
    Connection.WriteStrData('', IEST);
  end;
end;

procedure TdmACBrNFe.CustomCalculoImpostoCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('VBC', dtFloat, 15, '', '');
  Connection.WriteField('VICMS', dtFloat, 15, '', '');
  Connection.WriteField('VBCST', dtFloat, 15, '', '');
  Connection.WriteField('VST', dtFloat, 15, '', '');
  Connection.WriteField('VProd', dtFloat, 15, '', '');
  Connection.WriteField('VFrete', dtFloat, 15, '', '');
  Connection.WriteField('VSeg', dtFloat, 15, '', '');
  Connection.WriteField('VDesc', dtFloat, 15, '', '');
  Connection.WriteField('VII', dtFloat, 15, '', '');
  Connection.WriteField('VIPI', dtFloat, 15, '', '');
  Connection.WriteField('VPIS', dtFloat, 15, '', '');
  Connection.WriteField('VCOFINS', dtFloat, 15, '', '');
  Connection.WriteField('VOutro', dtFloat, 15, '', '');
  Connection.WriteField('VNF', dtFloat, 15, '', '');
end;

procedure TdmACBrNFe.CustomCalculoImpostoCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Total.ICMSTot do
  begin
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VBC),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VICMS),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VBCST),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VST),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VProd),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VFrete),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VSeg),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VDesc),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VII),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VIPI),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VPIS),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VCOFINS),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VOutro),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VNF),0));
  end;
end;

procedure TdmACBrNFe.CustomCalculoImpostoCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomDadosProdutosCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('CProd', dtString, 60, '', ''); //Codigo
  Connection.WriteField('XProd', dtString, 120, '', ''); //Descricao
  Connection.WriteField('NCM', dtString, 8, '', ''); //NCM
  Connection.WriteField('CFOP', dtString, 4, '', ''); //CFOP
  Connection.WriteField('UCom', dtString, 6, '', ''); //Unidade
  Connection.WriteField('QCom', dtFloat, 12, '', ''); //Quantidade
  Connection.WriteField('VUnCom', dtFloat, 16, '', ''); //ValorUnitario
  Connection.WriteField('VProd', dtFloat, 15, '', ''); //ValorTotal
  Connection.WriteField('CST', dtString, 2, '', ''); //CST
  Connection.WriteField('VBC', dtFloat, 15, '', ''); //ValorBase
  Connection.WriteField('PICMS', dtFloat, 5, '', ''); //Aliquota
  Connection.WriteField('VICMS', dtFloat, 15, '', ''); //Valor
  Connection.WriteField('VIPI', dtFloat, 15, '', ''); //Valor IPI
  Connection.WriteField('PIPI', dtFloat, 5, '', ''); //Aliquota IPI
end;

procedure TdmACBrNFe.CustomDadosProdutosCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := FNFe.Det.Count;
end;

procedure TdmACBrNFe.CustomDadosProdutosCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Det.Items[Connection.DataIndex] do
  begin
    with Prod do
    begin
      Connection.WriteStrData('', CProd);
      Connection.WriteStrData('', XProd);
      Connection.WriteStrData('', NCM);
      Connection.WriteStrData('', CFOP);
      Connection.WriteStrData('', UCom);
      Connection.WriteFloatData('', QCom);
      Connection.WriteFloatData('', VUnCom);
      Connection.WriteFloatData('', VProd);
      with Imposto.ICMS do
      begin
        if CST = cst00 then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst00));
            Connection.WriteFloatData('', vBC);
            Connection.WriteFloatData('', pICMS);
            Connection.WriteFloatData('', vICMS);
          end
        else if CST = cst10 then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst10));
            Connection.WriteFloatData('', VBC);
            Connection.WriteFloatData('', PICMS);
            Connection.WriteFloatData('', VICMS);
          end
        else if CST = cst20 then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst20));
            Connection.WriteFloatData('', VBC);
            Connection.WriteFloatData('', PICMS);
            Connection.WriteFloatData('', VICMS);
          end
        else if CST = cst30 then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst30));
            Connection.WriteFloatData('', VBCST);
            Connection.WriteFloatData('', PICMSST);
            Connection.WriteFloatData('', VICMSST);
          end
        else if (CST = cst40) or (CST = cst41) or (CST = cst50) then
          begin
            if (CST = cst40) then
               Connection.WriteStrData('', CSTICMSToStr(cst40))
            else if (CST = cst41) then
               Connection.WriteStrData('', CSTICMSToStr(cst41))
            else if (CST = cst40) then
               Connection.WriteStrData('', CSTICMSToStr(cst50));
            Connection.WriteFloatData('', 0);
            Connection.WriteFloatData('', 0);
            Connection.WriteFloatData('', 0);
          end
        else if (CST = cst51) then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst51));
            Connection.WriteFloatData('', VBC);
            Connection.WriteFloatData('', PICMS);
            Connection.WriteFloatData('', VICMS);
          end
        else if (CST = cst60) then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst60));
            Connection.WriteFloatData('', VBCST);
            Connection.WriteFloatData('', 0);
            Connection.WriteFloatData('', VICMSST);
          end
        else if (CST = cst70) then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst70));
            Connection.WriteFloatData('', VBC);
            Connection.WriteFloatData('', PICMS);
            Connection.WriteFloatData('', VICMS);
          end
        else if (CST = cst90) then
          begin
            Connection.WriteStrData('', CSTICMSToStr(cst90));
            Connection.WriteFloatData('', VBC);
            Connection.WriteFloatData('', PICMS);
            Connection.WriteFloatData('', VICMS);
          end;
      end;

      with Imposto.IPI do
      begin
        Connection.WriteFloatData('', 0);
        Connection.WriteFloatData('', 0);
{        if (IPITrib.CST = '00') or (IPITrib.CST = '49') or
           (IPITrib.CST = '50') or (IPITrib.CST = '99') then
          begin
            Connection.WriteStrData('', IPITrib.CST);
            Connection.WriteFloatData('', StringToFloatDef(IPITrib.VBC,0));
            Connection.WriteFloatData('', StringToFloatDef(IPITrib.PIPI,0));
            Connection.WriteFloatData('', StringToFloatDef(IPITrib.VIPI,0));
          end
        else if (IPINT.CST = '01') or (IPINT.CST = '02') or (IPINT.CST = '03') or
                (IPINT.CST = '04') or (IPINT.CST = '51') or (IPINT.CST = '52') or
                (IPINT.CST = '53') or (IPINT.CST = '54') or (IPINT.CST = '55') then
          begin
            Connection.WriteStrData('', IPINT.CST);
            Connection.WriteFloatData('', 0);
            Connection.WriteFloatData('', 0);
            Connection.WriteFloatData('', 0);
          end}
      end;
    end;
  end;
end;

procedure TdmACBrNFe.CustomTransportadorCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('ModFrete', dtString, 1, '', '');
  Connection.WriteField('CNPJCPF', dtString,18,'','');
  Connection.WriteField('XNome', dtString,60,'','');
  Connection.WriteField('IE', dtString,14,'','');
  Connection.WriteField('XEnder', dtString,60,'','');
  Connection.WriteField('XMun', dtString,60,'','');
  Connection.WriteField('UF', dtString,2,'','');
end;

procedure TdmACBrNFe.CustomTransportadorCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Transp do
  begin
    Connection.WriteStrData('', IfThen(modFrete = mfContaEmitente,'0','1'));
    with Transporta do
    begin
      if NaoEstaVazio(CNPJCPF) then
        Connection.WriteStrData('', FormatarCNPJ(CNPJCPF))
      else
        Connection.WriteStrData('', FormatarCPF(CNPJCPF));
      Connection.WriteStrData('', XNome);
      Connection.WriteStrData('', IE);
      Connection.WriteStrData('', XEnder);
      Connection.WriteStrData('', ParseText(XMun,true));
      Connection.WriteStrData('', UF);
    end;
  end;
end;

procedure TdmACBrNFe.CustomTransportadorCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomParametrosCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('Mensagem0', dtString, 60, '', '');
  Connection.WriteField('Imagem', dtBlob, 60, '', '');
end;

procedure TdmACBrNFe.CustomParametrosCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomParametrosCXNGetRow(
  Connection: TRvCustomConnection);
var
  vStream: TMemoryStream;
begin
  with FNFe.Ide do
    Connection.WriteStrData('', IfThen(TpAmb = taHomologacao,'Nota Fiscal sem valor Fiscal', ''));

  vStream := TMemoryStream.Create;
  try
    if NaoEstaVazio(FLogo) then
    begin
      if FileExists(FLogo) then
        vStream.LoadFromFile(FLogo);
    end;
    vStream.Position := 0;
    Connection.WriteBlobData(vStream.Memory^, vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure TdmACBrNFe.CustomIdentificacaoCXNGetCols(
  Connection: TRvCustomConnection);
begin
  //Ide
//  Connection.WriteField('Versao', dtString, 4, '', '');
  Connection.WriteField('Id', dtString, 44, '', '');
  Connection.WriteField('Chave', dtString, 60, '', '');
  Connection.WriteField('CUF', dtString, 2, '', '');
  Connection.WriteField('CNF', dtString, 9, '', '');
  Connection.WriteField('NatOp', dtString, 60, '', '');
  Connection.WriteField('IndPag', dtString, 1, '', '');
  Connection.WriteField('Mod_', dtString, 2, '', '');
  Connection.WriteField('Serie', dtString, 3, '', '');
  Connection.WriteField('NNF', dtString, 11, '', '');
  Connection.WriteField('DEmi', dtString, 10, '', '');
  Connection.WriteField('DSaiEnt', dtString, 10, '', '');
  Connection.WriteField('TpNF', dtString, 1, '', '');
  Connection.WriteField('CMunFG', dtString, 7, '', '');
  Connection.WriteField('TpImp', dtString, 1, '', '');
  Connection.WriteField('TpEmis', dtString, 1, '', '');
  Connection.WriteField('CDV', dtString, 1, '', '');
  Connection.WriteField('TpAmb', dtString, 1, '', '');
  Connection.WriteField('FinNFe', dtString, 1, '', '');
  Connection.WriteField('ProcEmi', dtString, 1, '', '');
  Connection.WriteField('VerProc', dtString, 20, '', '');
end;

procedure TdmACBrNFe.CustomIdentificacaoCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.infNFe do
  begin
//    Connection.WriteStrData('', IntToStr(Versao));
    Connection.WriteStrData('', OnlyNumber(Id));
    Connection.WriteStrData('', NotaUtil.FormatarChaveAcesso(Id));
  end;

  with FNFe.Ide do
  begin
    Connection.WriteStrData('', inttostr(CUF));
    Connection.WriteStrData('', inttostr(CNF));
    Connection.WriteStrData('', NatOp);
    Connection.WriteStrData('', IfThen(IndPag = ipVista,'0', IfThen(IndPag = ipPrazo,'1','2')));
    Connection.WriteStrData('', inttostr(Modelo));
    Connection.WriteStrData('', inttostr(Serie));
    Connection.WriteStrData('', FormatarNumeroDocumentoFiscal(inttostr(NNF)));
    Connection.WriteStrData('', FormatDate(datetostr(DEmi)));
    Connection.WriteStrData('', IfThen(NaoEstaVazio(datetostr(DSaiEnt)), FormatDate(datetostr(DSaiEnt))));
    Connection.WriteStrData('', IfThen(TpNF=tnEntrada,'0','1'));
    Connection.WriteStrData('', inttostr(CMunFG));
    Connection.WriteStrData('', IfThen(TpImp=tiRetrato,'1','2'));
    Connection.WriteStrData('', IfThen(TpEmis=teNormal,'1','5'));
    Connection.WriteStrData('', inttostr(CDV));
    Connection.WriteStrData('', IfThen(TpAmb = taHomologacao,'2','1'));
    Connection.WriteStrData('', IfThen(FinNFe=fnNormal,'1',IfThen(FinNFe=fnComplementar,'2','3')));
    Connection.WriteStrData('', IfThen(ProcEmi=peAplicativoContribuinte,'0',''));
    Connection.WriteStrData('', VerProc);
  end;
end;

procedure TdmACBrNFe.CustomIdentificacaoCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomDuplicatasCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('NDup', dtString, 60, '', '');
  Connection.WriteField('DVenc', dtString, 10, '', '');
  Connection.WriteField('VDup', dtFloat, 15, '', '');
end;

procedure TdmACBrNFe.CustomDuplicatasCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Cobr.Dup[Connection.DataIndex] do
  begin
    Connection.WriteStrData('', NDup);
    Connection.WriteStrData('', FormatDate(datetostr(DVenc)));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VDup),0));
  end;
end;

procedure TdmACBrNFe.CustomDuplicatasCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := FNFe.Cobr.Dup.Count;
end;

procedure TdmACBrNFe.CustomVeiculoCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('PLACA', dtString,8,'','');
  Connection.WriteField('UF', dtString,2,'','');
  Connection.WriteField('RNTC', dtString,20,'','');
end;

procedure TdmACBrNFe.CustomVeiculoCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Transp.VeicTransp do
  begin
    Connection.WriteStrData('', Placa);
    Connection.WriteStrData('', UF);
    Connection.WriteStrData('', RNTC);
  end;
end;

procedure TdmACBrNFe.CustomVeiculoCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomVolumesCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('QVol', dtFloat,15,'','');
  Connection.WriteField('Esp', dtString,60,'','');
  Connection.WriteField('Marca', dtString,60,'','');
  Connection.WriteField('NVol', dtString,60,'','');
  Connection.WriteField('PesoL', dtFloat,15,'','');
  Connection.WriteField('PesoB', dtFloat,15,'','');
end;

procedure TdmACBrNFe.CustomVolumesCXNGetRow(
  Connection: TRvCustomConnection);
begin
  with FNFe.Transp.Vol[Connection.DataIndex] do
  begin
    Connection.WriteFloatData('', StringToFloatDef(inttostr(QVol),0));
    Connection.WriteStrData('', Esp);
    Connection.WriteStrData('', Marca);
    Connection.WriteStrData('', NVol);
    Connection.WriteFloatData('', StringToFloatDef(floattostr(PesoL),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(PesoB),0));
  end;
end;

procedure TdmACBrNFe.CustomVolumesCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := FNFe.Transp.Vol.Count;
end;

procedure TdmACBrNFe.CustomInformacoesAdicionaisCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('OBS', dtMemo,5000,'','');
end;

procedure TdmACBrNFe.CustomInformacoesAdicionaisCXNGetRow(
  Connection: TRvCustomConnection);
var
  i: Integer;
  vTemp: TStringList;
  vStream: TMemoryStream;

  IndexCampo:Integer;
  Campos: ArrOfStrarray of string;
begin
  with FNFe.InfAdic do
  begin
    vTemp := TStringList.Create;
    vStream := TMemoryStream.Create;
    try
      for i:=0  to ObsCont.Count-1 do
      begin
        with ObsCont.Items[i] do
          vTemp.Add(XCampo+': '+XTexto);
      end;

      Campos := explode(';',InfCpl);
      for indexCampo:=0 to Length(Campos)-1 do
         vTemp.Add(Campos[indexCampo]);

      vTemp.SaveToStream(vStream);
      vStream.Position := 0;
      Connection.WriteBlobData(vStream.Memory^, vStream.Size);
    finally
      vTemp.Free;
      vStream.Free;
    end;
  end;
end;

procedure TdmACBrNFe.CustomInformacoesAdicionaisCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomObservacaoFiscoCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('OBSF', dtMemo,5000,'','');
end;

procedure TdmACBrNFe.CustomObservacaoFiscoCXNGetRow(
  Connection: TRvCustomConnection);
var
  i: Integer;
  vTemp: TStringList;
  vStream: TMemoryStream;
begin
  with FNFe.InfAdic do
  begin
    vTemp := TStringList.Create;
    vStream := TMemoryStream.Create;
    try
      for i:=0  to ObsFisco.Count-1 do
      begin
        with ObsFisco.Items[i] do
          vTemp.Add(XCampo+': '+XTexto);
      end;

      vTemp.Add(InfAdFisco);
      vTemp.SaveToStream(vStream);
      vStream.Position := 0;
      Connection.WriteBlobData(vStream.Memory^, vStream.Size);
    finally
      vTemp.Free;
      vStream.Free;
    end;
  end;

end;

procedure TdmACBrNFe.CustomObservacaoFiscoCXNOpen(
  Connection: TRvCustomConnection);
begin
  Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomISSQNCXNGetCols(
  Connection: TRvCustomConnection);
begin
  Connection.WriteField('vSERV', dtFloat, 15, '', '');
  Connection.WriteField('vBC', dtFloat, 15, '', '');
  Connection.WriteField('vISS', dtFloat, 15, '', '');
end;

procedure TdmACBrNFe.CustomISSQNCXNGetRow(Connection: TRvCustomConnection);
begin
  with FNFe.Total.ISSQNtot do
  begin
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VServ),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VBC),0));
    Connection.WriteFloatData('', StringToFloatDef(floattostr(VISS),0));
  end;
end;

procedure TdmACBrNFe.CustomISSQNCXNOpen(Connection: TRvCustomConnection);
begin
   if ((FNFe.Total.ISSQNtot.VServ=0) and
       (FNFe.Total.ISSQNtot.VBC=0) and
       (FNFe.Total.ISSQNtot.VISS=0)) then
      Connection.DataRows := 0
   else
      Connection.DataRows := 1;
end;

procedure TdmACBrNFe.CustomEmitenteCXNEOF(Connection: TRvCustomConnection;
  var Eof: Boolean);
begin

end;

end.
