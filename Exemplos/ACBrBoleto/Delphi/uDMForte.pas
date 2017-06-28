unit uDMForte;

interface

uses
  SysUtils, Classes, ACBrBoleto, ACBrBase, ACBrBoletoFCFortesFr;

type
  TdmForte = class(TDataModule)
    ACBrBoletoReport: TACBrBoletoFCFortes;
    ACBrBoleto: TACBrBoleto;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmForte: TdmForte;

implementation


{$R *.dfm}

end.
