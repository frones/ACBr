unit uDMForte;

interface

uses
  System.SysUtils, System.Classes, ACBrBoleto, ACBrBase, ACBrBoletoFCFortesFr;

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

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
