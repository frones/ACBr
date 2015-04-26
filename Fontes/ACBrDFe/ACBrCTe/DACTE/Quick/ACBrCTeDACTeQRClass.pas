{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
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
|*  - Implementado fqrDACTeQRRetrato.ProtocoloNFE( sProt ).
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: Caique Rodrigues
|*  - Doação units para geração do DANFe via QuickReport
*******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTeQRClass;

interface

uses
 Forms, SysUtils, Classes, QRPrntr,
 pcnConversao, pcteCTe, ACBrCTeDACTeClass,
 ACBrCTeDACTeQR, ACBrCTeDACTeQRRetrato, ACBrCTeDACTeQRRetratoA5,
 ACBrCTeDACTeQRRetrato2Vias,
 ACBrCTeDAEventoQR, ACBrCTeDAEventoQRRetrato,
 ACBrCTeDAInutQR, ACBrCTeDAInutQRRetrato;

type
  TACBrCTeDACTeQR = class(TACBrCTeDACTeClass)
  private
    FPosRecibo: TPosRecibo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDACTe(CTe: TCTe = nil); override;
    procedure ImprimirDACTePDF(CTe: TCTe = nil); override;
    procedure ImprimirEVENTO(CTe: TCTe = nil); override;
    procedure ImprimirEVENTOPDF(CTe: TCTe = nil); override;
    procedure ImprimirINUTILIZACAO(CTe: TCTe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(CTe: TCTe = nil); override;
  published
    property PosRecibo: TPosRecibo read FPosRecibo write FPosRecibo default prCabecalho;
  end;

implementation

uses
  StrUtils, Dialogs, ACBrUtil, ACBrCTe;

var
  frmCTeDAEventoQR : TfrmCTeDAEventoQR;
  frmCTeDAInutQR : TfrmCTeDAInutQR;

constructor TACBrCTeDACTeQR.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
end;

destructor TACBrCTeDACTeQR.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrCTeDACTeQR.ImprimirDACTe(CTe: TCTe = nil);
var
  i: Integer;
  sProt: String;
  frmDACTeQRRetrato: TfrmDACTeQR; //TfrmDACTeQRRetrato;
begin
  case TamanhoPapel of
    tpA5: begin
           frmDACTeQRRetrato := TfrmDACTeQRRetratoA5.Create(Self);
          {$IFDEF QReport_PDF}
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A5Trans;
          {$ELSE}
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A5;
          {$ENDIF}
           frmDACTeQRRetrato.QRCTe.Page.Length    := 148.0;
           frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
          end;
    tpA4_2vias: begin
                 frmDACTeQRRetrato := TfrmDACTeQRRetrato2vias.Create(Self);
                 frmDACTeQRRetrato.QRCTe.Page.PaperSize := A4;
                 frmDACTeQRRetrato.QRCTe.Page.Length    := 297.0;
                 frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
                end;
     else begin // tpA4
           frmDACTeQRRetrato := TfrmDACTeQRRetrato.Create(Self);
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A4;
           frmDACTeQRRetrato.QRCTe.Page.Length    := 297.0;
           frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
          end;
  end;

//  frmDACTeQRRetrato := TfrmDACTeQRRetrato.Create(Self);
  sProt := TACBrCTe(ACBrCTe).DACTe.ProtocoloCTe;
//  frmDACTeQRRetrato.ProtocoloCTe(sProt);

  if CTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count - 1 do
      frmDACTeQRRetrato.Imprimir(TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe,
                                 Logo, Email, ImprimirHoraSaida, ExpandirLogoMarca,
                                 ImprimirHoraSaida_Hora, ResumoCanhoto, Fax, NumCopias,
                                 Sistema, Site, Usuario, MostrarPreview, MargemSuperior,
                                 MargemInferior, MargemEsquerda, MargemDireita,
                                 Impressora, PosRecibo, CTeCancelada, EPECEnviado);
  end
  else
    frmDACTeQRRetrato.Imprimir(CTe, Logo, Email, ImprimirHoraSaida, ExpandirLogoMarca,
                               ImprimirHoraSaida_Hora, ResumoCanhoto, Fax, NumCopias,
                               Sistema, Site, Usuario, MostrarPreview, MargemSuperior,
                               MargemInferior, MargemEsquerda, MargemDireita,
                               Impressora, PosRecibo, CTeCancelada, EPECEnviado);

  if frmDACTeQRRetrato.QRCTe <> nil then
    frmDACTeQRRetrato.Free;
end;

procedure TACBrCTeDACTeQR.ImprimirDACTePDF(CTe: TCTe = nil);
var
  i: Integer;
  sProt: String;
  NomeArq: String;
  frmDACTeQRRetrato: TfrmDACTeQR; //TfrmDACTeQRRetrato;
begin
  case TamanhoPapel of
    tpA5: begin
           frmDACTeQRRetrato := TfrmDACTeQRRetratoA5.Create(Self);
          {$IFDEF QReport_PDF}
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A5Trans;
          {$ELSE}
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A5;
          {$ENDIF}
           frmDACTeQRRetrato.QRCTe.Page.Length    := 148.0;
           frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
          end;
    tpA4_2vias: begin
                 frmDACTeQRRetrato := TfrmDACTeQRRetrato2vias.Create(Self);
                 frmDACTeQRRetrato.QRCTe.Page.PaperSize := A4;
                 frmDACTeQRRetrato.QRCTe.Page.Length    := 297.0;
                 frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
                end;
     else begin // tpA4
           frmDACTeQRRetrato := TfrmDACTeQRRetrato.Create(Self);
           frmDACTeQRRetrato.QRCTe.Page.PaperSize := A4;
           frmDACTeQRRetrato.QRCTe.Page.Length    := 297.0;
           frmDACTeQRRetrato.QRCTe.Page.Width     := 210.0;
          end;
  end;

//  frmDACTeQRRetrato := TfrmDACTeQRRetrato.Create(Self);
  sProt := TACBrCTe(ACBrCTe).DACTe.ProtocoloCTe;
//  frmDACTeQRRetrato.ProtocoloCTe( sProt );

  if CTe = nil then
  begin
    for i := 0 to TACBrCTe(ACBrCTe).Conhecimentos.Count-1 do
    begin
      NomeArq := StringReplace(TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe.infCTe.Id, 'CTe', '', [rfIgnoreCase]);
//        NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '.pdf';
      NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-cte.pdf';

      frmDACTeQRRetrato.SavePDF(NomeArq, TACBrCTe(ACBrCTe).Conhecimentos.Items[i].CTe,
                                Logo, Email, ImprimirHoraSaida, ExpandirLogoMarca,
                                ImprimirHoraSaida_Hora, ResumoCanhoto, Fax, NumCopias,
                                Sistema, Site, Usuario, MargemSuperior, MargemInferior,
                                MargemEsquerda, MargemDireita, PosRecibo,
                                CTeCancelada, EPECEnviado);
    end;
  end
  else
  begin
    NomeArq := StringReplace(CTe.infCTe.Id, 'CTe', '', [rfIgnoreCase]);
//     NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '.pdf';
    NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-cte.pdf';

    frmDACTeQRRetrato.SavePDF(NomeArq, CTe, Logo, Email, ImprimirHoraSaida,
                              ExpandirLogoMarca, ImprimirHoraSaida_Hora, ResumoCanhoto,
                              Fax, NumCopias, Sistema, Site, Usuario, MargemSuperior,
                              MargemInferior, MargemEsquerda, MargemDireita,
                              PosRecibo, CTeCancelada, EPECEnviado);
  end;

  if frmDACTeQRRetrato.QRCTe <> nil then
    frmDACTeQRRetrato.Free;
end;

procedure TACBrCTeDACTeQR.ImprimirEVENTO(CTe: TCTe);
var
  i, j: Integer;
  Impresso: Boolean;
begin
  frmCTeDAEventoQR := TfrmCTeDAEventoQRRetrato.Create(Self);

  try
    if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
    begin
      for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
      begin
        Impresso := False;

        for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
        begin
//          if Copy(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id, 4, 44) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
          if StringReplace(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id, 'CTe', '', [rfIgnoreCase]) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
          begin
            frmCTeDAEventoQR.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                      FLogo, FNumCopias, FSistema, FUsuario,
                                      FMostrarPreview, FMargemSuperior,
                                      FMargemInferior, FMargemEsquerda,
                                      FMargemDireita, FImpressora,
                                      TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
            Impresso := True;
            Break;
          end;
        end;

        if Impresso = False then
        begin
          frmCTeDAEventoQR.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                    FLogo, FNumCopias, FSistema, FUsuario,
                                    FMostrarPreview, FMargemSuperior,
                                    FMargemInferior, FMargemEsquerda,
                                    FMargemDireita, FImpressora);
        end;
      end;
    end
    else
    begin
      for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
        begin
          frmCTeDAEventoQR.Imprimir(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                    FLogo, FNumCopias, FSistema, FUsuario,
                                    FMostrarPreview, FMargemSuperior,
                                    FMargemInferior, FMargemEsquerda,
                                    FMargemDireita, FImpressora);
        end;
    end;
  finally
    FreeAndNil(frmCTeDAEventoQR);
  end;
end;

procedure TACBrCTeDACTeQR.ImprimirEVENTOPDF(CTe: TCTe);
var
  i, j: Integer;
  NomeArq: String;
  Impresso: Boolean;
begin
  frmCTeDAEventoQR := TfrmCTeDAEventoQRRetrato.Create(Self);

  try
    if TACBrCTe(ACBrCTe).Conhecimentos.Count > 0 then
    begin
      for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
      begin
//          NomeArq := TACBrCTe(ACBrCTe).DACTE.PathPDF +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 09, 44) +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 03, 06) +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 53, 02) + 'evento.pdf';
        NomeArq := TACBrCTe(ACBrCTe).DACTe.PathPDF +
                   StringReplace(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id, 'ID', '', [rfIgnoreCase]) +
                   '-procEventoCTe.pdf';
        Impresso := False;

        for j := 0 to (TACBrCTe(ACBrCTe).Conhecimentos.Count - 1) do
        begin
//          if Copy(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id, 4, 44) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
          if StringReplace(TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe.infCTe.Id, 'CTe', '', [rfIgnoreCase]) = TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.chCTe then
          begin
            frmCTeDAEventoQR.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                     FLogo, NomeArq, FSistema, FUsuario,
                                     FMargemSuperior, FMargemInferior,
                                     FMargemEsquerda, FMargemDireita,
                                     TACBrCTe(ACBrCTe).Conhecimentos.Items[j].CTe);
            Impresso := True;
            Break;
          end;
        end;

        if Impresso = False then
        begin
          frmCTeDAEventoQR.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                   FLogo, NomeArq, FSistema, FUsuario,
                                   FMargemSuperior, FMargemInferior,
                                   FMargemEsquerda, FMargemDireita);
        end;
      end;
    end
    else
    begin
      for i := 0 to (TACBrCTe(ACBrCTe).EventoCTe.Evento.Count - 1) do
       begin
//          NomeArq := TACBrCTe(ACBrCTe).DACTE.PathPDF +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 09, 44) +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 03, 06) +
//                   Copy(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.id, 53, 02) + 'evento.pdf';
         NomeArq := TACBrCTe(ACBrCTe).DACTe.PathPDF +
                    StringReplace(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i].InfEvento.Id, 'ID', '', [rfIgnoreCase]) +
                    '-procEventoCTe.pdf';

         frmCTeDAEventoQR.SavePDF(TACBrCTe(ACBrCTe).EventoCTe.Evento.Items[i],
                                  FLogo, NomeArq, FSistema, FUsuario,
                                  FMargemSuperior, FMargemInferior,
                                  FMargemEsquerda, FMargemDireita);
       end;
    end;
  finally
    FreeAndNil(frmCTeDAEventoQR);
  end;
end;

procedure TACBrCTeDACTeQR.ImprimirINUTILIZACAO(CTe: TCTe);
var
 i, j: Integer;
 Impresso: Boolean;
begin
  frmCTeDAInutQR := TfrmCTeDAInutQRRetrato.Create(Self);

  try
    frmCTeDAInutQR.Imprimir(TACBrCTe(ACBrCTe),
                            FLogo, FNumCopias, FSistema, FUsuario,
                            FMostrarPreview, FMargemSuperior,
                            FMargemInferior, FMargemEsquerda,
                            FMargemDireita, FImpressora);

  finally
    FreeAndNil(frmCTeDAInutQR);
  end;
end;

procedure TACBrCTeDACTeQR.ImprimirINUTILIZACAOPDF(CTe: TCTe);
var
  i, j: Integer;
  NomeArq: String;
  Impresso: Boolean;
begin
  frmCTeDAInutQR := TfrmCTeDAInutQRRetrato.Create(Self);

  try
    NomeArq := StringReplace(TACBrCTe(ACBrCTe).InutCTe.Id, 'ID', '', [rfIgnoreCase]);
    if NomeArq = '' then
      NomeArq := StringReplace(TACBrCTe(ACBrCTe).InutCTe.RetInutCTe.Id, 'ID', '', [rfIgnoreCase]);
    NomeArq := PathWithDelim(Self.PathPDF) + NomeArq + '-procInutCTe.pdf';

    frmCTeDAInutQR.SavePDF(TACBrCTe(ACBrCTe),
                           FLogo, NomeArq, FSistema, FUsuario,
                           FMargemSuperior, FMargemInferior,
                           FMargemEsquerda, FMargemDireita);

  finally
    FreeAndNil(frmCTeDAInutQR);
  end;
end;

end.

