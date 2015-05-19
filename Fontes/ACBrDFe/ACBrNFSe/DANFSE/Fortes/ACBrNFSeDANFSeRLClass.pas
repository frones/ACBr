{$I ACBr.inc}

unit ACBrNFSeDANFSeRLClass;

interface

uses
 Forms, SysUtils, Classes, pnfsNFSe, ACBrNFSeDANFSeClass;

type
  TACBrNFSeDANFSeRL = class( TACBrNFSeDANFSeClass )
   private
   // Augusto Fontana
   protected
     FPrintDialog: Boolean;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFSe(NFSe : TNFSe = nil); override ;
    procedure ImprimirDANFSePDF(NFSe : TNFSe = nil); override ;
   // Augusto Fontana
   published
     property PrintDialog: Boolean read FPrintDialog write FPrintDialog;
  end;

implementation

uses
 StrUtils, Dialogs, ACBrUtil, ACBrNFSe, ACBrNFSeDANFSeRLRetrato;

constructor TACBrNFSeDANFSeRL.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  // Augusto Fontana
  FPrintDialog := True;
end;

destructor TACBrNFSeDANFSeRL.Destroy;
begin
  inherited Destroy ;
end;


procedure TACBrNFSeDANFSeRL.ImprimirDANFSe(NFSe : TNFSe = nil);
var
 i : Integer;
 frlDANFSeRLRetrato : TfrlDANFSeRLRetrato;
begin
 frlDANFSeRLRetrato := TfrlDANFSeRLRetrato.Create(Self);

 frlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.QuebradeLinha);

 if NFSe = nil
  then begin
   for i:= 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count-1 do
    begin
     frlDANFSeRLRetrato.Imprimir(  TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe
                                 , Logo
                                 , Email
                                 , Fax
                                 , NumCopias
                                 , Sistema
                                 , Site
                                 , Usuario
                                 , MostrarPreview
                                 , MargemSuperior
                                 , MargemInferior
                                 , MargemEsquerda
                                 , MargemDireita
                                 , Impressora
                                 , PrestLogo
                                 , Prefeitura
                                 , RazaoSocial
                                 , Endereco
                                 , Complemento
                                 , Fone
                                 , Municipio
                                 , InscMunicipal
                                 , EMail_Prestador
                                 , UF
                                 , T_InscEstadual
                                 , T_InscMunicipal
                                 , OutrasInformacaoesImp
                                 , Atividade
                                 , T_Fone
                                 , T_Endereco
                                 , T_Complemento
                                 , T_Email
                                 , PrintDialog);
    end;
  end
  else frlDANFSeRLRetrato.Imprimir(  NFSe
                                   , Logo
                                   , Email
                                   , Fax
                                   , NumCopias
                                   , Sistema
                                   , Site
                                   , Usuario
                                   , MostrarPreview
                                   , MargemSuperior
                                   , MargemInferior
                                   , MargemEsquerda
                                   , MargemDireita
                                   , Impressora
                                   , PrestLogo
                                   , Prefeitura
                                   , RazaoSocial
                                   , Endereco
                                   , Complemento
                                   , Fone
                                   , Municipio
                                   , InscMunicipal
                                   , EMail_Prestador
                                   , UF
                                   , T_InscEstadual
                                   , T_InscMunicipal
                                   , OutrasInformacaoesImp
                                   , Atividade
                                   , T_Fone
                                   , T_Endereco
                                   , T_Complemento
                                   , T_Email
                                   , PrintDialog);

 frlDANFSeRLRetrato.Free;
end;

procedure TACBrNFSeDANFSeRL.ImprimirDANFSePDF(NFSe : TNFSe = nil);
var
 NomeArq : String;
 i : Integer;
 frlDANFSeRLRetrato : TfrlDANFSeRLRetrato;
begin
 frlDANFSeRLRetrato := TfrlDANFSeRLRetrato.Create(Self);

 frlDANFSeRLRetrato.QuebradeLinha(TACBrNFSe(ACBrNFSe).Configuracoes.WebServices.QuebradeLinha);

 if NFSe = nil
  then begin
   for i:= 0 to TACBrNFSe(ACBrNFSe).NotasFiscais.Count-1 do
    begin
//     NomeArq :=  trim(TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NomeArq);
//     if NomeArq=''
//      then begin
       NomeArq := StringReplace(TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe.Numero,'NFSe', '', [rfIgnoreCase]);
       NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'.pdf';
//      end
//      else NomeArq := StringReplace(NomeArq,'-nfse.xml', '.pdf', [rfIgnoreCase]);

{
class procedure TfrlDANFSeRL.SavePDF(AFile: String; ANFSe: TNFSe; ALogo, AEmail,
  AFax: String; ANumCopias: Integer; ASistema, ASite, AUsuario: String;
  AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita: Double;
  APrestLogo, APrefeitura, ARazaoSocial, AEndereco, AComplemento, AFone, AMunicipio,
  AInscMunicipal, AEMail_Prestador, AUF, AT_InscEstadual, AT_InscMunicipal,
  AOutrasInformacaoesImp, AAtividade : String);
}

     frlDANFSeRLRetrato.SavePDF( NomeArq
                               , TACBrNFSe(ACBrNFSe).NotasFiscais.Items[i].NFSe
                               , Logo
                               , Email
                               , Fax
                               , NumCopias
                               , Sistema
                               , Site
                               , Usuario
                               , MargemSuperior
                               , MargemInferior
                               , MargemEsquerda
                               , MargemDireita
                               , PrestLogo
                               , Prefeitura
                               , RazaoSocial
                               , Endereco
                               , Complemento
                               , Fone
                               , Municipio
                               , InscMunicipal
                               , EMail_Prestador
                               , UF
                               , T_InscEstadual
                               , T_InscMunicipal
                               , OutrasInformacaoesImp
                               , Atividade
                               , T_Fone
                               , T_Endereco
                               , T_Complemento
                               , T_Email);
    end;
  end
  else begin
   NomeArq := StringReplace(NFSe.Numero,'NFSe', '', [rfIgnoreCase]);
   NomeArq := PathWithDelim(Self.PathPDF)+NomeArq+'.pdf';

   frlDANFSeRLRetrato.SavePDF( NomeArq
                             , NFSe
                             , Logo
                             , Email
                             , Fax
                             , NumCopias
                             , Sistema
                             , Site
                             , Usuario
                             , MargemSuperior
                             , MargemInferior
                             , MargemEsquerda
                             , MargemDireita
                             , PrestLogo
                             , Prefeitura
                             , RazaoSocial
                             , Endereco
                             , Complemento
                             , Fone
                             , Municipio
                             , InscMunicipal
                             , EMail_Prestador
                             , UF
                             , T_InscEstadual
                             , T_InscMunicipal
                             , OutrasInformacaoesImp
                             , Atividade
                             , T_Fone
                             , T_Endereco
                             , T_Complemento
                             , T_Email);
  end;

 frlDANFSeRLRetrato.Free;
end;

end.
