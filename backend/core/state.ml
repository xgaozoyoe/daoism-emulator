type 'a state = {
  description: string;
  deliver: ((Object.t Feature.t) * Object.t option) array;
  state: 'a;
}


