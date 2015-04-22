{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
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

{*******************************************************************************
|* Historico
|* 11/12/2009: Emerson Crema
|*  - Implementado fqrDANFeQRRetrato.ProtocoloNFE( sProt ).
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: Caique Rodrigues
|*  - Doação units para geração do Danfe via QuickReport
|* 23/11/2010: Peterson de Cerqueira Matos
|*  - Tratamento das propriedades "CasasDecimais._qCom", "CasasDecimais._vUnCom",
|*    "Impressora"
|* 20/05/2011: Peterson de Cerqueira Matos
|*  - Tratamento da propriedade "ExibirResumoCanhoto_Texto"
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeQRClass;

interface

uses
 Forms, SysUtils, Classes,
 pcnNFe, pcnConversao, ACBrNFeDANFEClass,
 ACBrNFeDANFeQR, ACBrNFeDAEventoQR, ACBrNFeDAInutQR;

type
  TACBrNFeDANFEQR = class( TACBrNFeDANFEClass )
   private
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); override;
    procedure ImprimirEVENTO(NFe: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(NFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAO(NFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(NFe: TNFe = nil); override;
  end;

implementation

uses
 StrUtils, Dialogs,
 ACBrUtil, ACBrNFe,
 ACBrNFeDANFeQRRetrato, ACBrNFeDANFeQRPaisagem, ACBrNFeDANFeQRSimplificado,
 ACBrNFeDANFeQRNFCe, ACBrNFeDANFeQRNFCeA4, ACBrNFeDAEventoQRRetrato,
 ACBrNFeDAInutQRRetrato;

var
 frmNFeDAEventoQR : TfrmNFeDAEventoQR;
 frmNFeDAInutQR : TfrmNFeDAInutQR;

constructor TACBrNFeDANFEQR.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
end;

destructor TACBrNFeDANFEQR.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrNFeDANFEQR.ImprimirDANFE(NFE : TNFe = nil);
var
  i : Integer;
  fqrDANFeQRRetrato : TfqrDANFeQR; //TfqrDANFeQRRetrato;
  sProt : String;
begin
  case TipoDANFe of
    tiRetrato:      fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self);
    tiPaisagem:     fqrDANFeQRRetrato := TfqrDANFeQRPaisagem.Create(Self);
    tiSimplificado: fqrDANFeQRRetrato := TfqrDANFeQRSimplificado.Create(Self);
    tiNFCe:         fqrDANFeQRRetrato := TfqrDANFeQRNFCe.Create(Self);
    tiNFCeA4:       fqrDANFeQRRetrato := TfqrDANFeQRNFCeA4.Create(Self);
    else            fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self); // tiRetrato
  end;

//  fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self);
  sProt := TACBrNFe(ACBrNFe).DANFE.ProtocoloNFe;
//  fqrDANFeQRRetrato.ProtocoloNFE( sProt );

  if NFE = nil then
   begin
     for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count-1 do
      begin
        fqrDANFeQRRetrato.Imprimir(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe,
                                   TACBrNFe(ACBrNFe), Logo, Email,
                                   ExibirResumoCanhoto, Fax, NumCopias, Sistema,
                                   Site, Usuario, MostrarPreview, MargemSuperior,
                                   MargemInferior, MargemEsquerda, MargemDireita,
                                   CasasDecimais._qCom, CasasDecimais._vUnCom,
                                   Impressora, ExibirResumoCanhoto_Texto,
                                   ExpandirLogoMarca, NFeCancelada,
                                   LocalImpCanhoto, ImprimeItens, ViaConsumidor,
                                   vTroco, ImprimirDescPorc,
                                   ImprimirDetalhamentoEspecifico,
                                   ImprimirTotalLiquido,
                                   CasasDecimais._Mask_qCom,
                                   CasasDecimais._Mask_vUnCom);
      end;
   end
  else
     fqrDANFeQRRetrato.Imprimir(NFe, TACBrNFe(ACBrNFe), Logo, Email,
                                ExibirResumoCanhoto, Fax, NumCopias, Sistema,
                                Site, Usuario, MostrarPreview, MargemSuperior,
                                MargemInferior, MargemEsquerda, MargemDireita,
                                CasasDecimais._qCom, CasasDecimais._vUnCom,
                                Impressora, ExibirResumoCanhoto_Texto,
                                ExpandirLogoMarca, NFeCancelada, LocalImpCanhoto,
                                ImprimeItens, ViaConsumidor, vTroco,
                                ImprimirDescPorc, ImprimirDetalhamentoEspecifico,
                                ImprimirTotalLiquido,
                                CasasDecimais._Mask_qCom,
                                CasasDecimais._Mask_vUnCom);

  fqrDANFeQRRetrato.Free;
end;

procedure TACBrNFeDANFEQR.ImprimirDANFEPDF(NFE : TNFe = nil);
var
  NomeArq : String;
  i : Integer;
  fqrDANFeQRRetrato : TfqrDANFeQR; //TfqrDANFeQRRetrato;
  sProt : String;
begin
  case TipoDANFe of
    tiRetrato:      fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self);
    tiPaisagem:     fqrDANFeQRRetrato := TfqrDANFeQRPaisagem.Create(Self);
    tiSimplificado: fqrDANFeQRRetrato := TfqrDANFeQRSimplificado.Create(Self);
    tiNFCe:         fqrDANFeQRRetrato := TfqrDANFeQRNFCe.Create(Self);
    tiNFCeA4:       fqrDANFeQRRetrato := TfqrDANFeQRNFCeA4.Create(Self);
    else            fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self); // tiRetrato
  end;

//  fqrDANFeQRRetrato := TfqrDANFeQRRetrato.Create(Self);
  sProt := TACBrNFe(ACBrNFe).DANFE.ProtocoloNFe;
//  fqrDANFeQRRetrato.ProtocoloNFE( sProt );

  if NFE = nil then
   begin
     for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count-1 do
      begin
        NomeArq := StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]);
        NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-nfe.pdf';

        fqrDANFeQRRetrato.SavePDF(NomeArq, TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe,
                                  TACBrNFe(ACBrNFe), Logo, Email, ExibirResumoCanhoto,
                                  Fax, NumCopias, Sistema, Site, Usuario,
                                  MargemSuperior, MargemInferior, MargemEsquerda,
                                  MargemDireita, CasasDecimais._qCom,
                                  CasasDecimais._vUnCom, ExibirResumoCanhoto_Texto,
                                  ExpandirLogoMarca, NFeCancelada, LocalImpCanhoto,
                                  ImprimeItens, ViaConsumidor, vTroco,
                                  ImprimirDescPorc, ImprimirDetalhamentoEspecifico,
                                  ImprimirTotalLiquido,
                                  CasasDecimais._Mask_qCom,
                                  CasasDecimais._Mask_vUnCom);
      end;
   end
   else
   begin
     NomeArq := StringReplace(NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]);
     NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'-nfe.pdf';

     fqrDANFeQRRetrato.SavePDF(NomeArq, NFe, TACBrNFe(ACBrNFe), Logo, Email,
                               ExibirResumoCanhoto, Fax, NumCopias, Sistema,
                               Site, Usuario, MargemSuperior, MargemInferior,
                               MargemEsquerda, MargemDireita, CasasDecimais._qCom,
                               CasasDecimais._vUnCom, ExibirResumoCanhoto_Texto,
                               ExpandirLogoMarca, NFeCancelada, LocalImpCanhoto,
                               ImprimeItens, ViaConsumidor, vTroco,
                               ImprimirDescPorc, ImprimirDetalhamentoEspecifico,
                               ImprimirTotalLiquido,
                               CasasDecimais._Mask_qCom,
                               CasasDecimais._Mask_vUnCom);
   end;

  fqrDANFeQRRetrato.Free;
end;

procedure TACBrNFeDANFEQR.ImprimirEVENTO(NFe: TNFe);
var
 i, j: Integer;
 Impresso: Boolean;
begin
  frmNFeDAEventoQR := TfrmNFeDAEventoQRRetrato.Create(Self);

  if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          Impresso := False;
          for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
            begin
//              if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
              if StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
                begin
                  frmNFeDAEventoQR.Imprimir(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                            FLogo, FNumCopias, FSistema,
                                            FUsuario, FMostrarPreview,
                                            FMargemSuperior, FMargemInferior,
                                            FMargemEsquerda, FMargemDireita,
                                            FImpressora,
                                            TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe);
                  Impresso := True;
                  Break;
                end;
            end;

          if Impresso = False then
            begin
              frmNFeDAEventoQR.Imprimir(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                        FLogo, FNumCopias, FSistema, FUsuario,
                                        FMostrarPreview, FMargemSuperior,
                                        FMargemInferior, FMargemEsquerda,
                                        FMargemDireita, FImpressora);
            end;
        end;
    end
  else
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          frmNFeDAEventoQR.Imprimir(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                    FLogo, FNumCopias, FSistema, FUsuario,
                                    FMostrarPreview, FMargemSuperior,
                                    FMargemInferior, FMargemEsquerda,
                                    FMargemDireita, FImpressora);
        end;
    end;

  FreeAndNil(frmNFeDAEventoQR);
end;

procedure TACBrNFeDANFEQR.ImprimirEVENTOPDF(NFe: TNFe);
var
 i, j: Integer;
 NomeArq: String;
 Impresso: Boolean;
begin
  frmNFeDAEventoQR := TfrmNFeDAEventoQRRetrato.Create(Self);

  if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
//          NomeArq := TACBrNFe(ACBrNFe).DANFe.PathPDF +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 09, 44) +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 03, 06) +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 53, 02) + 'evento.pdf';
          NomeArq := TACBrNFe(ACBrNFe).DANFe.PathPDF +
                     StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 'ID', '', [rfIgnoreCase]) +
                     '-procEventoNFe.pdf';

          Impresso := False;

          for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
            begin
//              if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
              if StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
                begin
                  frmNFeDAEventoQR.SavePDF(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                           FLogo, NomeArq, FSistema, FUsuario,
                                           FMargemSuperior, FMargemInferior,
                                           FMargemEsquerda, FMargemDireita,
                                           TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe);
                  Impresso := True;
                  Break;
                end;
            end;

          if Impresso = False then
            begin
              frmNFeDAEventoQR.SavePDF(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                       FLogo, NomeArq, FSistema, FUsuario,
                                       FMargemSuperior, FMargemInferior,
                                       FMargemEsquerda, FMargemDireita);
            end;
        end;
    end
  else
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
//          NomeArq := TACBrNFe(ACBrNFe).DANFe.PathPDF +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 09, 44) +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 03, 06) +
//                   Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 53, 02) + 'evento.pdf';
          NomeArq := TACBrNFe(ACBrNFe).DANFe.PathPDF +
                     StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 'ID', '', [rfIgnoreCase]) +
                     '-procEventoNFe.pdf';

          frmNFeDAEventoQR.SavePDF(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                                   FLogo, NomeArq, FSistema, FUsuario,
                                   FMargemSuperior, FMargemInferior,
                                   FMargemEsquerda, FMargemDireita);
        end;
    end;

  FreeAndNil(frmNFeDAEventoQR);
end;

procedure TACBrNFeDANFEQR.ImprimirINUTILIZACAO(NFe: TNFe);
begin
  frmNFeDAInutQR := TfrmNFeDAInutQRRetrato.Create(Self);

  frmNFeDAInutQR.Imprimir(TACBrNFe(ACBrNFe),
                          FLogo, FNumCopias, FSistema, FUsuario,
                          FMostrarPreview, FMargemSuperior,
                          FMargemInferior, FMargemEsquerda,
                          FMargemDireita, FImpressora);

  FreeAndNil(frmNFeDAInutQR);
end;

procedure TACBrNFeDANFEQR.ImprimirINUTILIZACAOPDF(NFe: TNFe);
var
 NomeArq: String;
begin
  frmNFeDAInutQR := TfrmNFeDAInutQRRetrato.Create(Self);

  NomeArq := StringReplace(TACBrNFe(ACBrNFe).InutNFe.ID, 'ID', '', [rfIgnoreCase]);
  if NomeArq = '' then
    NomeArq := StringReplace(TACBrNFe(ACBrNFe).InutNFe.InutNFe.Id, 'ID', '', [rfIgnoreCase]);
  NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-procInutNFe.pdf';

  frmNFeDAInutQR.SavePDF(TACBrNFe(ACBrNFe),
                         FLogo, NomeArq, FSistema, FUsuario,
                         FMargemSuperior, FMargemInferior,
                         FMargemEsquerda, FMargemDireita);

  FreeAndNil(frmNFeDAInutQR);
end;

end.
