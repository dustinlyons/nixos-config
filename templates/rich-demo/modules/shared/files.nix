{ pkgs, config, ... }:

let
  githubPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOk8iAnIaa1deoc7jw8YACPNVka1ZFJxhnU4G74TmS+p dustin@Dustins-MBP.localdomain";
  githubPublicSigningKey = ''
    -----BEGIN PGP PUBLIC KEY BLOCK-----

    mQGNBGNHB2YBDADNAoEzFeTEn/84dnrZKL+yeOq0m07cMFwQLiiylstJj0OxOJI3
    0frjNsijIOTDhtrrYNr+vkc7Bsf2P4aI+FmkrBfKfY4oA1GBjyb823ran99Fnfy9
    r7n8FM7X/6E7BG8cYawcLmFW5A8++h25tqoEoSw9y0ENTC/tP5TSZc7ypUJ2qKs5
    nfvnCYs7P2avLtJrElZiwnkjsMADyj6CtGjTOAGi5LypsDX/9oqzAMOJH6eD2829
    irhZ9zLg1HLkaFN4FApdmeHhCyM8e3d4yXMYAfjQ52RFFci4cf+cVp2ijgX+FZpp
    7aBz9Fxqfb34kCzPktXh6dROmlFg9Of6jJmcGBxDr7vuo6FciFyQUjSe1BsMIjrb
    WC5N4wb/nWGUPaWKtN7BTUNcTGy5xAk4i03xWacamqaLbMiqKN9BHoGT8D7BmqQo
    toh1yhoVpuKkwOT66NM7vfCH5N3s0zEsAI8RHHSqNBWincx5yyQoqveeYPn9EOJs
    f7MnPR2mgvBuvN8AEQEAAbQgRHVzdGluIEx5b25zIDxkdXN0aW5AZGx5b25zLmRl
    dj6JAdEEEwEIADsWIQSRE59mup65UqK9X4TZWq5A0U5jswUCY0cHZgIbAwULCQgH
    AgIiAgYVCgkICwIEFgIDAQIeBwIXgAAKCRDZWq5A0U5js1kxDACQZAP6orX+4tWO
    dk+9gNtKlq+oDYwFg6ITl8NyurCzlLl3OhhKuCIhCd6FeBhcmCO2WhupKgkjB2ij
    HCUMlf4Qs6gLHgU+MvvtwIJYycil0q10FATRv2jH73txk4hCUcSgy4MNT6MsjOgB
    innZgFYte08a54SHxmRN5RbXCeddkcDM+kdeMsEu24kczxbNHjkJGV2IpyWYIH5m
    Y+VPySt6url4UQZhtF00weV21Nl3yao3+lqv+f/ML0EFJTyri6TzH9E9Owk/iszz
    hhFoofPRvvqE4VkvnwUmHidzWa9x3XyuzwBFRTBgE6ZfsDDclRUmhNsxRtjwSW0k
    FmjUDmCgWjlGY5iJneJ32n5ccwWc5MBLztHb8u52eg74f84iMr0wSYctaWDb++nl
    pB64jEJobZWXJf74zHkIb51TfhSAqGGX6gHxQ/bsZ3iv8zYXWkjTsq4dgtbylWVA
    suhaqxTG8/WjCzFLCQebME7x3ChEJFNXM40LMi3pBLPTge0UCUK5AY0EY0cHZgEM
    ANqEI67q5MRDcGnX0gKeKgRcqMFlJq0Lpm1YfqjVBiw4PEwQBJ8cW3nZaA+fTZTJ
    1X31ti+0HkcYbnQzsXDAFNo+iaeJ3JDMgIK5+tayCpTFnjec47iniP2wIaPfdaGx
    zqMEp9JXAJuwpjT5qIqIyx9Qh6fvteittz2FKycla3mnrAeswyFLM0LsjkUi7g0O
    FLcmOiCEmcQQzL9cKLPm2p+tnwudId5FdeQtDXW9wYN+kEu+UMOGFVzrCCtWMoee
    NNna9ZPw/5Pjk2RbMSykvGvImcUQeKtheyV/xk8i9NUdTQk6hctK7dGm45QlvroQ
    95cHdEKUdJRgzpN8TG+LWPR8+FUFATlSNFCTPNJiaVY1Jyn74Prfg/V7TkFNZbSP
    KRMYQy9BfUxC1uGsy/a5NlfPAJ+uU7up+NHD9GCl7QtmJGsqdkac8VCSpUt+dgCI
    ILlIHbeWsMBsMZUNggOHZt+G8xE13mo2yr6ylJ87sRA0iu9Yk2BgQ1zkiLBPwZ+y
    UQARAQABiQG2BBgBCAAgFiEEkROfZrqeuVKivV+E2VquQNFOY7MFAmNHB2YCGwwA
    CgkQ2VquQNFOY7NLjQwAuCZYL+I5QwJ4nTFRRtkJYi55BvLbEuyVnYwbkHpHksg6
    Nxh1gbykEdFAafJAVDCwU/ov+GA7RLVRS0TtnU7DBKUmzbO6MvFusjs8190PwLKP
    9Eb2gWgTkECyd0WC3HMvfTBk96koidpxGLDal5P7B8DoanaqcuEf5QAWawT66lW/
    sOYmrDOlEisV14/Mk/XgdOO/X/BKDXoGlTOtsiWFw50sBzjg9nKQUkaSzgU1HB5g
    TSZu6Wi4OtVdTMxT2ryOLj78YAQ3eBtfDak2in2J6bOY2i9d+vP5TKik4DeZypNQ
    iLgAKJ5+2NRlCbnci1bmay21Ke1PIZiUTe82lCoS4CoEJzKU89NtHSU64M7FEjBS
    5yYtMrs+ko+INWYG9aEj7rs4grpQMP9NF5AxfDuq77+Ca7Vg9pTkI1DYj1D91mWR
    J/pMd3YqlIkZ4JBN489FZ1qqRV6RuKko/qyqvvQ5+ziqrh+QjluJU4qI60znX/LI
    1USIqi8ymF08Ak+cIhyO
    =WFfO
    -----END PGP PUBLIC KEY BLOCK-----
  '';
in

{
  # Initializes Emacs with org-mode so we can tangle the main config
  #
  # @todo: Get rid of this after we've upgraded to Emacs 29 on the Macbook
  # Emacs 29 includes org-mode now
  ".emacs.d/init.el" = {
    text = builtins.readFile ./config/emacs/init.el;
  };

  ".ssh/id_github.pub" = {
    text = githubPublicKey;
  };

  ".ssh/pgp_github.pub" = {
    text = githubPublicSigningKey;
  };
}
